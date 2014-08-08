(* -*- tuareg -*- *)

open Yojson.Safe
open Lwt
open Eliom_registration
open Eliom_service
open Eliom_lib
open Eliom_parameter
open ExtPervasives

type ('a, 'b) ty =
  | TUnit : (unit, unit) ty
  | TString : string -> (string, [ `One of string ] param_name) ty
  | TFile : string -> (file_info, [ `One of file_info ] param_name) ty
  | TPair : ('a, 'ap) ty * ('b, 'bp) ty -> (('a * 'b), ('ap * 'bp)) ty
  | TList : string * ('a, 'ap) ty -> ('a list, 'ap listnames) ty

let ( ** ) a b = TPair (a, b)

let string s = TString s

let file s = TFile s

let list name ty = TList (name, ty)

let unit = TUnit

type ('i, 'ip, 'o, 'op) api_service = {
  name  : string;
  mname : string;
  ity   : ('i, 'ip) ty;
  oty   : ('o, 'op) ty;
  doc   : string;
}

type some_api_service =
    ApiService : ('i, 'ip, 'o, 'op) api_service -> some_api_service

let apis = Hashtbl.create 37

let rec eliom_parameters_from_ty
: type a b. (a, b) ty -> (a, [ `WithoutSuffix ], b) Eliom_parameter.params_type
= Eliom_parameter.(function
  | TString l -> string l
  | TFile f -> file f
  | TList (name, t) -> list name (eliom_parameters_from_ty t)
  | TUnit -> unit
  | TPair (a, b) -> eliom_parameters_from_ty a ** eliom_parameters_from_ty b
)

(** To return the content of a file, prefer standard ways. *)
let file_info_to_json_object f = `String "file"

let rec ty_to_json_object
: type a b. (a, b) ty -> a -> (string * json) list
= fun ty x ->
  match ty with
    | TString l -> [(l, `String x)]
    | TFile f -> [(f, file_info_to_json_object f)]
    | TList (f, ty) -> [(f, `List (List.map (ty_to_json ty) x))]
    | TUnit -> []
    | TPair (a, b) -> ty_to_json_object a (fst x) @ ty_to_json_object b (snd x)

and ty_to_json
: type a b. (a, b) ty -> a -> json
= fun ty x -> `Assoc (ty_to_json_object ty x)

let init_api_service name mname ity oty doc register =
  let s = { name; mname; ity; oty; doc } in
  Hashtbl.add apis name (ApiService s);
  let post_params = eliom_parameters_from_ty ity in
  let fallback = ErrorHTTP.fallback name in
  register post_params fallback

exception APIError of string

let api_service name mname ity oty doc code =
  init_api_service name mname ity oty doc (fun post_params fallback ->
    Eliom_registration.String.register_post_service
      ~https:true
      ~fallback
      ~post_params
      (fun () p ->
        try_lwt
          lwt r = code p in
          Lwt.return (to_string (ty_to_json oty r) ^ "\n", "application/json")
        with (APIError msg) ->
          Lwt.return (msg ^ "\n", "content/txt"))
  )

let api_download_service name mname ity doc code =
  init_api_service name mname ity (TFile "o") doc (fun post_params fallback ->
    Eliom_registration.File.register_post_service
      ~https:true
      ~fallback
      ~post_params
      (fun () p -> code p)
  )

let success s = return (s)

let error s = raise_lwt (APIError (ErrorHTTP.msg s))

let completed () = success "completed"

let return_completed _ = return (`OK "completed")

let handle_error = function
  | `OK x -> return x
  | `KO (`AlreadyExists _) -> assert false
  | `KO (`UndefinedEntity id) -> error "undefined_entity"
  | `KO (`SystemError e) -> error ("system:" ^ e)
  | `KO (`InternalError e) -> error ("internal:" ^ (Printexc.to_string e))

let file_upload_service import =
  let service =
    Eliom_service.Http.post_coservice'
      ~post_params:(Eliom_parameter.(file "file"))
      ()
  in
  Eliom_registration.Action.register ~options:`NoReload ~service
    (fun () file ->
      Ocsigen_messages.errlog
        (Printf.sprintf "Received a file (%Ld):\n %s\n%s\n%s"
           file.Ocsigen_extensions.filesize
           file.Ocsigen_extensions.tmp_filename
           file.Ocsigen_extensions.raw_original_filename
           file.Ocsigen_extensions.original_basename);
        (* FIXME: Handle error. *)
      lwt dest, commit = import file.Ocsigen_extensions.original_basename in
      ltry ExtUnix.(cp file.Ocsigen_extensions.tmp_filename dest)
      >> commit
    );
  service

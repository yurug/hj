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
  | TPair   : ('a, 'ap) ty * ('b, 'bp) ty -> (('a * 'b), ('ap * 'bp)) ty

let ( ** ) a b = TPair (a, b)

let string s = TString s

let file s = TFile s

let unit = TUnit

type ('i, 'ip, 'o, 'op) api_service = {
  name  : string;
  mname : string;
  ity   : ('i, 'ip) ty;
  oty   : ('o, 'op) ty;
  doc   : string;
  code  : 'i -> 'o Lwt.t
}

type some_api_service =
  ApiService : ('i, 'ip, 'o, 'op) api_service -> some_api_service

let apis = Hashtbl.create 37

let rec eliom_parameters_from_ty
: type a b. (a, b) ty -> (a, [ `WithoutSuffix ], b) Eliom_parameter.params_type
= Eliom_parameter.(function
  | TString l -> string l
  | TFile f -> file f
  | TUnit -> unit
  | TPair (a, b) -> eliom_parameters_from_ty a ** eliom_parameters_from_ty b
)

let file_info_to_json_object f = assert false

let rec ty_to_json_object
: type a b. (a, b) ty -> a -> (string * json) list
= fun ty x ->
  match ty with
    | TString l -> [(l, `String x)]
    | TFile f -> [(f, file_info_to_json_object f)]
    | TUnit -> []
    | TPair (a, b) -> ty_to_json_object a (fst x) @ ty_to_json_object b (snd x)

let ty_to_json ty x = to_string (`Assoc (ty_to_json_object ty x))

let api_service name mname ity oty doc code =
  let s = { name; mname; ity; oty; doc; code } in
  Hashtbl.add apis name (ApiService s);
  let post_params = eliom_parameters_from_ty ity in
  let fallback = ErrorHTTP.fallback name in
  Eliom_registration.String.register_post_service
    ~https:true
    ~fallback
    ~post_params
    (fun () p ->
      lwt r = code p in
      Lwt.return (ty_to_json oty r ^ "\n", "application/json"))

let success s = return (s)

let error s = return (ErrorHTTP.msg s)

let completed () = success "completed"

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

(* -*- tuareg -*- *)

open Lwt
open ExtPervasives
open ExtUnix
open HTTP

let create_resource_management_api
    (type d)
    (type c)
    (module E : Entity.S with type data = d and type change = c)
    access_control
    upload_name download_name ls_name
    module_name
=
  api_service upload_name module_name
    (string "identifier" ** string "resource_name" ** file "file")
    (string "status")
    "Upload a resource."
    (access_control (fun (id, (resource_name, file)) ->
      (E.make id >>>= fun e ->
       ltry (cat file.Ocsigen_extensions.tmp_filename) >>>= fun content ->
       let resource = Resource.make resource_name content in
       E.import_resource e resource
      ) >>= function
        | `OK _ -> completed ()
        | `KO (`AlreadyExists _) -> assert false
        | `KO (`UndefinedEntity id) -> error "undefined_entity"
        | `KO (`SystemError e) -> error ("system:" ^ e)
        | `KO (`InternalError e) -> error ("internal:" ^ (Printexc.to_string e))
    ))

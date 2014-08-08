(* -*- tuareg -*- *)

open Lwt
open ExtPervasives
open ExtUnix
open Identifier
open HTTP

let create_resource_management_api
    (type d)
    (type c)
    (module E : Entity.S with type data = d and type change = c)
    upload_name download_name ls_name
    module_name
=
  api_service upload_name module_name
    (string "identifier" ** string "resource_name" ** file "file")
    (string "status")
    "Upload a resource."
    (fun (id, (resource_name, file)) ->
      (E.make (identifier_of_string id) >>>= fun e ->
       ltry (cat file.Ocsigen_extensions.tmp_filename) >>>= fun content ->
       let resource = Resource.make resource_name content in
       E.import_resource e resource
       >>>= return_completed
      ) >>= handle_error
    ),

  api_download_service download_name module_name
    (string "identifier" ** string "resource_name")
    "Download a resource."
    (fun (id, resource_name) ->
      (E.make (identifier_of_string id) >>>= fun e ->
       E.resource e resource_name >>>= fun (_, p) ->
       return (`OK (Identifier.string_of_path p))
      ) >>= handle_error
    )

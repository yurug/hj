(* -*- tuareg -*- *)

open Lwt
open ExtPervasives
open ExtUnix
open Identifier
open HTTP

(* FIXME: Access control is missing for the moment. *)
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
       return (`OK (VFS.real_path p))
      ) >>= handle_error
    ),

  api_service ls_name module_name
    (string "identifier" ** string "options" ** string "filter")
    (list "resource_info" (
      string "name"
      ** string "version"
      ** string "author"
      ** string "last_modification")
    )
    "List resources."
    (fun (id, (options, filter)) -> Str.(
      let all_version = string_match (regexp "--all") options 0 in
      (E.make (identifier_of_string id) >>>= fun e ->
       let info name =
         if string_match (regexp filter) name 0 then
           E.resource e name >>= function
             | `KO _ ->
               return [(name, ("inconsistent", ("", "")))]
             | `OK (_, path) ->
               let version_info version =
                 lwt number = VFS.number version in
                 lwt author = VFS.author version in
                 lwt date   = VFS.date version in
                 return (name, (number, (author, date)))
               in
               lwt versions =
                 VFS.(if all_version then
                     versions path
                   else
                     (latest path >>= function
                       | `OK e -> return (`OK [e])
                       | `KO e -> return (`KO e))
                 ) >>= function
                   | `OK vs -> return vs
                   | `KO _ -> return [] (* FIXME: Handle error. *)
               in
               Lwt_list.map_s version_info versions
         else
           return []
       in
       lwt infos = Lwt_list.map_s info (E.resources e) in
       return (`OK (List.flatten infos))
      ) >>= handle_error
     ))

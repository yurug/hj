(* -*- tuareg -*- *)

open Lwt
open ExtPervasives
open ExtUnix
open Identifier
open HTTP

let _ = Random.self_init ()

(* FIXME: Access control is missing for the moment. *)
let create_resource_management_api
    ?(make_identifier = fun s -> return (`OK (identifier_of_string s)))
    (type d)
    (type c)
    (module E : Entity.S with type data = d and type change = c)
    upload_name upload_tar_name download_name
    ls_name publish_name download_public_name
    module_name
=
  api_service upload_name module_name
    (string "identifier" ** string "resource_name" ** file "file")
    (string "status")
    "Upload a resource."
    (fun (id, (resource_name, file)) ->
      (make_identifier id >>>= fun id ->
       E.make id >>>= fun e ->
       ltry (cat file.Ocsigen_extensions.tmp_filename) >>>= fun content ->
       let resource = Resource.make resource_name content in
       E.import_resource e resource
       >>>= return_completed
      ) >>= handle_error
    ),

  api_service upload_tar_name module_name
    (string "identifier" ** file "file")
    (string "status")
    "Upload resources using a tarball."
    (fun (id, file) ->
      (make_identifier id >>>= fun id ->
       E.make id >>>= (fun e ->
         let tar_tmp = Filename.(
           concat (get_temp_dir_name ()) (
             Printf.sprintf "hj%d.tar.dir" (Random.bits ())
           )
         )
         in
         ltry (mkdir tar_tmp) >>
           ltry (tar_expand file.Ocsigen_extensions.tmp_filename tar_tmp) >>
           ltry (ls tar_tmp) >>>= function filenames ->
             let uploaded = ref [] in
             let import filename =
               let resource_name = Filename.basename filename in
               ltry (cat filename) >>= function
                 | `OK content ->
                   let resource = Resource.make resource_name content in
                   E.import_resource e resource >>
                     return (uploaded := resource_name :: !uploaded)
                 | `KO _ -> return () (* FIXME *)
             in
             Lwt_list.iter_s import filenames
             >> ltry (rmdir ~content:true tar_tmp)
             >> return_success "uploaded")
      ) >>= handle_error
    ),

  api_download_service download_name module_name
    (string "identifier" ** string "resource_name" ** string "version")
    "Download a resource."
    (fun (id, (resource_name, version)) ->
      (E.make (identifier_of_string id) >>>= fun e ->
       let version = if version = "" then None else Some version in
       E.resource e ?version resource_name >>>= fun (r, p) ->
       let tmp = Filename.get_temp_dir_name () in
       let tmp_file = Filename.concat tmp resource_name in
       ltry (fun lraise -> echo (Resource.content r) tmp_file lraise)
       >>>= fun _ -> return (`OK tmp_file)
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
     )),

  api_service publish_name module_name
    (string "identifier" ** string "resource_name" ** string "status")
    (string "status")
    "Publish or unpublish a resource."
    (fun (id, (resource_name, status)) ->
      (make_identifier id >>>= fun id ->
       E.make id >>>= fun e -> begin
         match status with
           | "1" -> return (`OK (E.publish e true resource_name))
           | "0" -> return (`OK (E.publish e false resource_name))
           | _ -> return (`KO `InvalidParameter)
       end
       >>>= return_completed
      ) >>= handle_error
    ),

  Eliom_registration.File.register_service
    ~path:[download_public_name]
    ~get_params:Eliom_parameter.(suffix (all_suffix "id"))
    (fun id () ->
      let resource_name = List.(hd (rev id)) in
      let id = List.(rev (tl (rev id))) in
      let id = identifier_of_path (make (List.map label id)) in
      E.make id >>= function
        | `OK e ->
          if E.is_public_resource e resource_name then
            Lwt.return (OnDisk.resource_real_path id resource_name)
          else
            Lwt.return "/dev/null" (* FIXME: Right way to say go away? *)
        | `KO _ ->
          Lwt.return "/dev/null"
    )

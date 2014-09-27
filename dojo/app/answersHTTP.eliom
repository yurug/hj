(* -*- tuareg -*- *)

{shared{
open Lwt
}}

open ExtPervasives
open User
open Answers
open Identifier
open HTTP
open UserHTTP

let channels = Hashtbl.create 13

let get_answers_channel = server_function Json.t<string> (fun id ->
  Answers.make (identifier_of_string id) >>= function
    | `OK a ->
      let s = Lwt_stream.clone (Entity.channel a) in
      let s = Lwt_stream.map (fun _ -> ()) s in
      let c = Eliom_comet.Channel.create ~size:100 s in
      Hashtbl.add channels id c;
      return (Some c)
    | `KO _ -> return None (* FIXME *)
)

{client{
let on_each_update id =
  %get_answers_channel id >>= function
  | Some s -> return (fun f -> Lwt_stream.iter_s f s)
  | None -> return (fun f -> return ())
}}

let make_identifier exo_name =
  logged_user () >>>= fun user ->
  let exo_id = identifier_of_string exo_name in
  let user_id = User.identifier user in
  return (`OK (answers_identifier exo_id user_id))

let (upload_resource, upload_tar, download_resource,
     ls_resource, publish_resource, download_public_resource)
=
  EntityHTTP.create_resource_management_api
    ~make_identifier
    (module Answers)
    "answers_upload"
    "answers_upload_tar"
    "answers_download"
    "answers_ls"
    "answers_publish"
    "answers_resources"
    "answers"

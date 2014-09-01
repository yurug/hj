(* -*- tuareg -*- *)

open Lwt
open ExtPervasives
open User
open Answers
open Identifier
open HTTP
open UserHTTP

let make_identifier exo_name =
  logged_user () >>>= fun user ->
  let exo_id = identifier_of_string exo_name in
  let user_id = User.identifier user in
  return (`OK (answers_identifier exo_id user_id))

let (upload_resource, download_resource, ls_resource) =
  EntityHTTP.create_resource_management_api
    ~make_identifier
    (module Answers)
    "answers_upload"
    "answers_download"
    "answers_ls"
    "answers"

(* -*- tuareg -*- *)

open Lwt
open ExtPervasives
open User
open Exercise
open Identifier
open HTTP
open UserHTTP

let exercise_create = HTTP.(
  api_service "exercise_create" "exercise"
    (string "name")
    (string "status")
    "Create a fresh exercise."
    (fun name ->
      (teacher_only () >>>= fun user ->
       Exercise.create user name
      ) >>= function
        | `OK _ -> completed ()
        | `KO `NotLogged -> error "not_logged"
        | `KO (`AlreadyExists _) -> error "already_exists"
        | `KO (`SystemError e) -> error ("system:" ^ e)
        | `KO (`InternalError e) -> error ("internal:" ^ (Printexc.to_string e))
        | `KO `StudentsCannotCreateExercise -> error "teacher_only"
        | `KO `ForbiddenService -> error "teacher_only"
        | `KO (`UndefinedEntity id) ->
          error ("undefined:" ^ (string_of_identifier id)))
)

let (upload_resource, download_resource, ls_resource) =
  EntityHTTP.create_resource_management_api
    (module Exercise)
    "exercise_upload"
    "exercise_download"
    "exercise_ls"
    "exercise"

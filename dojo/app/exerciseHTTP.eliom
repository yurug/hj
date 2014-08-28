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
       Exercise.create user (identifier_of_string name)
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

let exercise_update = HTTP.(
  api_service "exercise_update" "exercise"
    (string "identifier")
    (string "status")
    "Trigger an exercise update from resource 'source.aka'."
    (fun name ->
      (teacher_only () >>>= fun user ->
       Exercise.make (identifier_of_string name) >>>= fun exo ->
       Exercise.update user exo
      ) >>= function
        | `OK _ -> completed ()
        | `KO (`InvalidCode e) -> success e
        | `KO `NotLogged -> error "not_logged"
        | `KO (`AlreadyExists _) -> error "already_exists"
        | `KO (`SystemError e) -> error ("system:" ^ e)
        | `KO (`InternalError e) -> error ("internal:" ^ (Printexc.to_string e))
        | `KO `StudentsCannotCreateExercise -> error "teacher_only"
        | `KO `ForbiddenService -> error "teacher_only"
        | `KO (`UndefinedEntity id) ->
          error ("undefined:" ^ (string_of_identifier id)))
)

let exercise_questions = HTTP.(
  api_service "exercise_questions" "exercise"
    (string "identifier")
    (string "status")
    "Return the questions for the logged user."
    (fun name ->
      (logged_user () >>>= fun user ->
       let uid = User.identifier user in
       let name = identifier_of_string name in
       Exercise.(questions name uid)
      ) >>= function
        | `OK e -> success e
        | `KO (`InvalidModule id) ->
          error ("invalid_module:" ^ string_of_identifier id)
        | `KO (`InvalidCode e) -> success e
        | `KO `NotLogged -> error "not_logged"
        | `KO (`AlreadyExists _) -> error "already_exists"
        | `KO (`SystemError e) -> error ("system:" ^ e)
        | `KO (`InternalError e) -> error ("internal:" ^ (Printexc.to_string e))
        | `KO `StudentsCannotCreateExercise -> error "teacher_only"
        | `KO `ForbiddenService -> error "teacher_only"
        | `KO (`UndefinedEntity id) ->
          error ("undefined:" ^ (string_of_identifier id)))
)

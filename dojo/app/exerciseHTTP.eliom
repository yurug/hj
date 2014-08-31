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
       Exercise.(questions name uid) >>>= fun qs ->
       return  (`OK (Questions.Txt.questions qs))
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

let answer name = Questions.(
  HTTP.user name string_to_answer answer_to_string answer_to_json
)

let exercise_subscribe = HTTP.(
  api_service "exercise_subscribe" "exercise"
    (string "identifier")
    (string "status")
    "Return the questions for the logged user."
    (fun name ->
      (logged_user () >>>= fun user ->
       let uid = User.identifier user in
       let id = identifier_of_string name in
       Exercise.make id >>>= fun exo ->
       Exercise.user_answers exo uid
      ) >>= function
        | `OK _ -> completed ()
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

let exercise_push_new_answer = HTTP.(
  api_service "exercise_push_new_answer" "exercise"
    (string "identifier"
     ** string "question_identifier"
     ** answer "answer"
    )
    (string "status")
    "Return the questions for the logged user."
    (fun (name, (qid, answer)) ->
      (logged_user () >>>= fun user ->
       let uid = User.identifier user in
       let id = identifier_of_string name in
       Exercise.answer id uid qid answer
      ) >>= function
        | `OK _ -> completed ()
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

let exercise_evaluation_state = HTTP.(
  api_service "exercise_evaluation_state" "exercise"
    (string "identifier"
     ** string "question_identifier"
    )
    (string "status")
    "Return the state of the evaluation for a question."
    (fun (name, qid) ->
       let string_of_evaluation_state = Questions.(function
         | EvaluationError _ -> assert false
         | EvaluationDone grade -> Questions.string_of_grade grade
         | EvaluationWaits -> "waiting..."
         | EvaluationHandled _ -> "processing..."
       )
       in
       let string_of_evaluation_error = Questions.(function
         | UnboundQuestion -> "unbound_question"
         | SyntaxErrorInAnswer -> "syntax_error_in_answer"
         | InvalidContextDescription -> "invalid_context_description"
         | NoAnswer -> "no_answer"
         | IncompatibleAnswer -> "incompatible_answer"
         | ErrorDuringGraderExecution -> "error_during_grader_execution"
       )
       in
       (logged_user () >>>= fun user ->
        let uid = User.identifier user in
        let id = identifier_of_string name in
        Exercise.evaluation_state id uid qid
       ) >>= function
         | `OK (Questions.EvaluationError e) ->
           error (string_of_evaluation_error e)
         | `OK e -> success (string_of_evaluation_state e)
         | `KO (`InvalidModule id) ->
           error ("invalid_module:" ^ string_of_identifier id)
         | `KO (`InvalidCode e) -> success e
         | `KO `NotLogged -> error "not_logged"
         | `KO (`AlreadyExists _) -> error "already_exists"
         | `KO (`SystemError e) -> error ("system:" ^ e)
         | `KO (`InternalError e) ->
           error ("internal:" ^ (Printexc.to_string e))
         | `KO `StudentsCannotCreateExercise -> error "teacher_only"
         | `KO `ForbiddenService -> error "teacher_only"
         | `KO (`UndefinedEntity id) ->
           error ("undefined:" ^ (string_of_identifier id)))
)

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
        | `KO `FailedLogin -> error "login_failed"
        | `KO (`AlreadyExists _) -> error "already_exists"
        | `KO (`SystemError e) -> error ("system:" ^ e)
        | `KO (`InternalError e) -> error ("internal:" ^ (Printexc.to_string e))
        | `KO `StudentsCannotCreateExercise -> error "teacher_only"
        | `KO `ForbiddenService -> error "teacher_only"
        | `KO (`UndefinedEntity id) ->
          error ("undefined:" ^ (string_of_identifier id)))
)

let (upload_resource, upload_tar, download_resource, ls_resource) =
  EntityHTTP.create_resource_management_api
    (module Exercise)
    "exercise_upload"
    "exercise_upload_tar"
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
        | `KO `FailedLogin -> error "login_failed"
        | `KO (`AlreadyExists _) -> error "already_exists"
        | `KO (`SystemError e) -> error ("system:" ^ e)
        | `KO (`InternalError e) -> error ("internal:" ^ (Printexc.to_string e))
        | `KO `StudentsCannotCreateExercise -> error "teacher_only"
        | `KO `ForbiddenService -> error "teacher_only"
        | `KO (`UndefinedEntity id) ->
          error ("undefined:" ^ (string_of_identifier id)))
)

let exercise_refresh_evaluations = HTTP.(
  api_service "exercise_refresh_evaluations" "exercise"
    (string "identifier")
    (string "status")
    "Refresh the evaluations of an exercise 'source.aka'."
    (fun name ->
      (teacher_only () >>>= fun _ ->
       Exercise.refresh_evaluations (identifier_of_string name)
      ) >>= function
        | `OK _ -> completed ()
        | `KO (`InvalidCode e) -> success e
        | `KO `NotLogged -> error "not_logged"
        | `KO (`InvalidModule _ ) -> error "module_loading"
        | `KO `FailedLogin -> error "login_failed"
        | `KO (`AlreadyExists _) -> error "already_exists"
        | `KO (`SystemError e) -> error ("system:" ^ e)
        | `KO (`InternalError e) -> error ("internal:" ^ (Printexc.to_string e))
        | `KO `StudentsCannotCreateExercise -> error "teacher_only"
        | `KO `ForbiddenService -> error "teacher_only"
        | `KO (`UndefinedEntity id) ->
          error ("undefined:" ^ (string_of_identifier id)))
)

let exercise_questions_function name =
  logged_user () >>>= fun user ->
  let uid = User.identifier user in
  Exercise.(questions name uid)

let exercise_questions = HTTP.(
  api_service "exercise_questions" "exercise"
    (string "identifier")
    (string "status")
    "Return the questions for the logged user."
    (fun name ->
      (exercise_questions_function (identifier_of_string name) >>>= fun qs ->
       return  (`OK (Questions.Txt.exercise qs))
      ) >>= function
        | `OK e -> success e
        | `KO (`InvalidModule id) ->
          error ("invalid_module:" ^ string_of_identifier id)
        | `KO (`InvalidCode e) -> success e
        | `KO `FailedLogin -> error "login_failed"
        | `KO `NotLogged -> error "not_logged"
        | `KO (`AlreadyExists _) -> error "already_exists"
        | `KO (`SystemError e) -> error ("system:" ^ e)
        | `KO (`InternalError e) -> error ("internal:" ^ (Printexc.to_string e))
        | `KO `StudentsCannotCreateExercise -> error "teacher_only"
        | `KO `ForbiddenService -> error "teacher_only"
        | `KO (`UndefinedEntity id) ->
          error ("undefined:" ^ (string_of_identifier id)))
)

let exercise_questions_latex = HTTP.(
  api_service "exercise_questions_latex" "exercise"
    (string "identifier")
    (string "status")
    "Return the questions for the logged user (as a LaTeX file)."
    (fun name ->
      (exercise_questions_function (identifier_of_string name) >>>= fun qs ->
       return  (`OK (QuestionsLaTeX.make qs))
      ) >>= function
        | `OK e -> success e
        | `KO (`InvalidModule id) ->
          error ("invalid_module:" ^ string_of_identifier id)
        | `KO (`InvalidCode e) -> success e
        | `KO `FailedLogin -> error "login_failed"
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

let get_user_answers name =
  logged_user () >>>= fun user ->
  let uid = User.identifier user in
  let id = identifier_of_string name in
  Exercise.make id >>>= fun exo ->
  Exercise.user_answers exo uid

let exercise_subscribe = HTTP.(
  api_service "exercise_subscribe" "exercise"
    (string "identifier")
    (string "status")
    "Return the questions for the logged user."
    (fun name ->
      get_user_answers name >>= function
        | `OK _ -> completed ()
        | `KO (`InvalidModule id) ->
          error ("invalid_module:" ^ string_of_identifier id)
        | `KO (`InvalidCode e) -> success e
        | `KO `NotLogged -> error "not_logged"
        | `KO `FailedLogin -> error "login_failed"
        | `KO (`AlreadyExists _) -> error "already_exists"
        | `KO (`SystemError e) -> error ("system:" ^ e)
        | `KO (`InternalError e) -> error ("internal:" ^ (Printexc.to_string e))
        | `KO `StudentsCannotCreateExercise -> error "teacher_only"
        | `KO `ForbiddenService -> error "teacher_only"
        | `KO (`UndefinedEntity id) ->
          error ("undefined:" ^ (string_of_identifier id)))
)

let push_new_answer_function (id, (qid : string), answer) =
  logged_user () >>>= fun user ->
  let uid = User.identifier user in
  Exercise.answer id uid qid answer

let push_new_choice_function (name, qid, choice) =
  let id = identifier_of_string name in
  push_new_answer_function (id, (qid : string), Questions.Choice choice)
  >>= function
    | `OK () -> return true
    | `KO _ -> return false (* FIXME: Return an error message. *)

let push_new_choice_server_function =
  server_function Json.t<string * string * int> push_new_choice_function

let push_new_choices_server_function =
  server_function Json.t<string * string * int list> (
    fun (name, qid, choices) ->
      let id = identifier_of_string name in
      push_new_answer_function (id, (qid : string), Questions.Choices choices)
      >>= function
        | `OK () -> return true
        | `KO _ -> return false (* FIXME: Return an error message. *)
  )

let push_new_values_server_function =
  server_function Json.t<string * string * string array> (
    fun (name, qid, vs) ->
      let vs = Array.to_list vs in
      let id = identifier_of_string name in
      push_new_answer_function (id, (qid : string), Questions.GivenValues vs)
      >>= function
        | `OK () -> return true
        | `KO _ -> return false (* FIXME: Return an error message. *)
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
      let id = identifier_of_string name in
      (push_new_answer_function (id, qid, answer)) >>= function
        | `OK _ -> completed ()
        | `KO (`InvalidModule id) ->
          error ("invalid_module:" ^ string_of_identifier id)
        | `KO (`InvalidCode e) -> success e
        | `KO `NotLogged -> error "not_logged"
        | `KO `FailedLogin -> error "login_failed"
        | `KO (`AlreadyExists _) -> error "already_exists"
        | `KO (`SystemError e) -> error ("system:" ^ e)
        | `KO (`InternalError e) -> error ("internal:" ^ (Printexc.to_string e))
        | `KO `StudentsCannotCreateExercise -> error "teacher_only"
        | `KO `ForbiddenService -> error "teacher_only"
        | `KO (`UndefinedEntity id) ->
          error ("undefined:" ^ (string_of_identifier id)))
)

{shared{

type public_criteria =
  | Automatic
  | UserDefined of string

type public_score = int * int

type public_trace_event =
  | Message of string

type public_grade = {
  scores : (public_criteria * public_score) list;
  trace  : public_trace_event list
}

type public_evaluation_state =
  | NoEvaluation
  | EvaluationDone of string * string list * int * public_grade
  | EvaluationBeingProcessed
  | EvaluationFailed

}}

let make_public_criteria = function
  | Questions.Automatic -> Automatic
  | Questions.UserDefined u -> UserDefined u

let make_public_trace_event = function
  | Questions.Message s -> Message s

let make_public_grade g =
  { scores =
      List.map
        (fun (c, s) -> (make_public_criteria c, s))
        g.Questions.scores;
    trace =
      List.map make_public_trace_event g.Questions.trace
  }

let make_public_evaluation_state = function
  | Questions.EvaluationError _ ->
    EvaluationFailed
  | Questions.EvaluationDone (id, tags, level, g) ->
    EvaluationDone (id, tags, level, make_public_grade g)
  | Questions.EvaluationWaits ->
    EvaluationBeingProcessed
  | Questions.EvaluationHandled _ ->
    EvaluationBeingProcessed

let exercise_evaluation_state_function (name, (qid : string)) =
  logged_user () >>>= fun user ->
  let uid = User.identifier user in
  let id = identifier_of_string name in
  Exercise.evaluation_state id uid qid

let exercise_evaluation_state_server_function =
  server_function Json.t<string * string> (fun (name, qid) ->
    exercise_evaluation_state_function (name, qid) >>= function
      | `OK s -> return (make_public_evaluation_state s)
      | `KO _ -> return EvaluationFailed
  )

let exercise_evaluation_state = HTTP.(
  api_service "exercise_evaluation_state" "exercise"
    (string "identifier" ** string "question_identifier")
    (string "status")
    "Return the state of the evaluation for a question."
    (fun (name, qid) ->
       let string_of_evaluation_state = Questions.(function
         | EvaluationError _ -> assert false
         | EvaluationDone (_, _, _, grade) -> Questions.string_of_grade grade
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
       exercise_evaluation_state_function (name, qid) >>= function
         | `OK (Questions.EvaluationError e) ->
           error (string_of_evaluation_error e)
         | `OK e -> success (string_of_evaluation_state e)
         | `KO (`InvalidModule id) ->
           error ("invalid_module:" ^ string_of_identifier id)
         | `KO (`InvalidCode e) -> success e
         | `KO `NotLogged -> error "not_logged"
         | `KO `FailedLogin -> error "login_failed"
         | `KO (`AlreadyExists _) -> error "already_exists"
         | `KO (`SystemError e) -> error ("system:" ^ e)
         | `KO (`InternalError e) ->
           error ("internal:" ^ (Printexc.to_string e))
         | `KO `StudentsCannotCreateExercise -> error "teacher_only"
         | `KO `ForbiddenService -> error "teacher_only"
         | `KO (`UndefinedEntity id) ->
           error ("undefined:" ^ (string_of_identifier id)))
)



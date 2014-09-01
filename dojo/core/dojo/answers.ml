open Lwt
open Entity
open InMemory
open Identifier
open ExtPervasives
open ExtProcess

let answers_module = "hackojo.answers <here@hackojo.org>"

let path_of_exercise_answers exercise_identifier =
  concat
    (path_of_identifier exercise_identifier)
    (from_strings ["answers"])

let answers_identifier exercise_identifier user_identifier =
  let path = path_of_exercise_answers exercise_identifier in
  identifier_of_path (concat path (path_of_identifier user_identifier))

type internal_state = {
  contributors : User.identifier list;
  exercise     : Identifier.t;
  questions    : Questions.t;
  answers      : Questions.answers;
  evaluations  : Questions.evaluations;
} deriving (Json)

type public_change =
  | NewAnswer of Questions.identifier * User.identifier * Questions.answer
  | NewEvaluation of Questions.identifier * User.identifier * Questions.answer
  | UpdateEvaluationState of Questions.identifier * Questions.evaluation_state

include Entity.Make (struct

  type data = internal_state deriving (Json)

  type change = public_change

  let string_of_change = function
    | NewAnswer (qid, uid, a) ->
      Printf.sprintf "new answer to %s by %s : %s"
        qid
        (string_of_identifier uid)
        (Questions.string_of_answer a)

    | NewEvaluation (qid, uid, a) ->
      Printf.sprintf "new evaluation of %s by %s : %s"
        qid
        (string_of_identifier uid)
        (Questions.string_of_answer a)

    | UpdateEvaluationState (qid, _) ->
      Printf.sprintf "update evaluation state of %s" qid

  let react state mdeps cs later =
    let apply_change content = Questions.(function
      | NewAnswer (qid, uid, answer) ->
        let evaluations =
          update_evaluation qid EvaluationWaits content.evaluations
        in
        later (NewEvaluation (qid, uid, answer))
        >> return { content with evaluations }

      | NewEvaluation (qid, uid, answer) -> Questions.(
        let update new_evaluation_state =
          later (UpdateEvaluationState (qid, new_evaluation_state))
        in
        let answers = new_answer content.answers qid answer in
        let exo_identifier = (InMemory.content state).exercise in
        let exo_real_path = OnDisk.resource_real_path exo_identifier in
        let answer_real_path =
          OnDisk.resource_real_path (InMemory.identifier state)
        in
        lwt evaluations =
          update_evaluations
            exo_real_path answer_real_path
            content.evaluations content.questions
            qid answer update
        in
        return { content with answers; evaluations }
      )

      | UpdateEvaluationState (qid, evaluation_state) ->
        let evaluations =
          Questions.update_evaluation qid evaluation_state content.evaluations
        in
        return { content with evaluations }
    )
    in
    (* FIXME: Common pattern to be factorized out. *)
    let content0 = content state in
    lwt content = Lwt_list.fold_left_s apply_change content0 cs in
    if content == content0 then
      return NoUpdate
    else
      return (UpdateContent content)

  let current_version = "1.0"

  let converters = []

end)

let answers_for id uid questions =
  let answers_id = answers_identifier id uid in
  make answers_id >>= function
    | `OK answers ->
      return (`OK answers)
    | `KO (`UndefinedEntity _ | `SystemError _) ->
      let content = {
        questions;
        exercise = id;
        answers = Questions.empty_answers;
        contributors = [uid];
        evaluations = Questions.empty_evaluations;
      }
      in
      let init = (content, InMemory.empty_dependencies, []) in
      make ~init answers_id
    | `KO err ->
      return (`KO err)

let push_new_answer answers qid uid a =
  change answers (NewAnswer (qid, uid, a))

let evaluation_state answers qid =
  observe answers (fun state ->
    return (Questions.evaluation_state (content state).evaluations qid)
  )

let answer_of_question answers qid =
  observe answers (fun state -> return (content state).answers)
  >>= fun answers -> return (Questions.lookup_answer answers qid)

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

(* Rajouter "history" et "forked_from" *)
type internal_state = {
  contributors : User.identifier list;
  exercise     : Identifier.t;
  description  : Questions.t;
  answers      : Questions.answers;
  evaluations  : Questions.evaluations;
} deriving (Json)

type public_change =
  | NewQuestions of Questions.t
  | NewAnswer of Questions.identifier * User.identifier * Questions.answer
  | NewEvaluation of Questions.identifier * User.identifier * Questions.answer
  | UpdateEvaluationState of Questions.identifier * Questions.evaluation_state
  | NewContributor of Identifier.t

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

    | NewQuestions qs ->
      Printf.sprintf "new questions"

    | NewContributor u ->
      Printf.sprintf "new contributor %s" (string_of_identifier u)

  let react state mdeps cs later =
    let apply_change content =
      let update_tags qid evaluation_state =
        Questions.on_completed evaluation_state (fun qid tags difficulty ->
          Lwt_list.iter_s
            (fun u -> User.tag u content.exercise qid tags difficulty)
            content.contributors
        )
      in
      Questions.(function
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
        let answers = new_answer content.answers qid answer uid in
        let exo_identifier = (InMemory.content state).exercise in
        let exo_real_path = OnDisk.resource_real_path exo_identifier in
        let answer_real_path =
          OnDisk.resource_real_path (InMemory.identifier state)
        in
        lwt evaluations, evaluation_state =
          update_evaluations
            exo_real_path answer_real_path
            content.evaluations
            content.description.questions
            qid answer update
        in
        update_tags qid evaluation_state
        >> return { content with answers; evaluations }
      )

      | UpdateEvaluationState (qid, evaluation_state) ->
        let current_evaluation_state =
          Questions.evaluation_state content.evaluations qid
        in
        if evaluation_state = current_evaluation_state then
          return content
        else (
          update_tags qid evaluation_state
          >> return {
            content with evaluations =
              Questions.update_evaluation qid evaluation_state content.evaluations
          }
        )

      | NewQuestions description ->
        (* FIXME: We must implement a caching system not to evaluate
           FIXME: twice the same answers on the same questions.
           FIXME: Yet, this caching process should be bypassable... *)
        iter_answers_s (fun qid (a, uid) ->
          later (NewEvaluation (qid, uid, a))
        ) content.answers
        >> return { content with description }

      | NewContributor uid ->
        return (
          if List.mem uid content.contributors then
            content
          else
            { content with contributors = uid :: content.contributors }
        )
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

let answers_for id uid description =
  let answers_id = answers_identifier id uid in
  make answers_id >>= function
    | `OK answers ->
      return (`OK answers)
    | `KO (`UndefinedEntity _ | `SystemError _) ->
      let content = {
        description;
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

let push_new_description answers d =
  change answers (NewQuestions d)

let push_new_answer answers qid uid a =
  change answers (NewAnswer (qid, uid, a))

let evaluation_state answers qid =
  observe answers (fun state ->
    return (Questions.evaluation_state (content state).evaluations qid)
  )

let answer_of_question answers qid =
  observe answers (fun state -> return (content state).answers)
  >>= fun answers -> return (Questions.lookup_answer answers qid)

let refresh_questions a qs =
  change a (NewQuestions qs)

let contributors answers =
  observe answers (fun state ->
    return (content state).contributors
  )

let add_contributor answers uid =
  make answers >>= function
    | `OK answers -> change answers (NewContributor uid)
    | `KO e -> return () (* FIXME *)

let import_contributor_answer dst_answers_id dst_uid src_answers_id qid =
  make src_answers_id >>= function
    | `OK src_answers ->
      lwt src_contributors = contributors src_answers in
      if List.mem dst_uid src_contributors then (
        (** Yes, dst_uid is an official contributor to src_answers *)
        make dst_answers_id >>= function
          | `OK dst_answers ->
            lwt src_answer, src_author = answer_of_question src_answers qid in
            begin
              match src_answer with
                | Questions.File f ->
                  begin resource src_answers f >>= function
                    | `OK (r, _) ->
                      import_resource dst_answers r
                      >> return () (* FIXME *)
                    | `KO _ ->
                      return () (* FIXME *)
                  end
                | _ -> return ()
            end
            >> (
              push_new_answer dst_answers qid src_author src_answer
            )
          | `KO _ ->
            return () (* FIXME *)
      ) else
        return () (* FIXME *)

    | `KO e ->
      return () (* FIXME *)

(** -*- tuareg -*- *)

(** Evaluation entities. *)

open Lwt

open CORE_entity
open CORE_identifier
open CORE_error_messages
open CORE_exercise
open CORE_answer
open CORE_context
open CORE_inmemory_entity
open COMMON_pervasives

{shared{

type job =
  | ExecutableJob of CORE_sandbox.job
deriving (Json)

type submission = string deriving (Json)

type evaluation_state =
  | Unevaluated
  | Evaluated      of CORE_context.score
  | BeingEvaluated of job * CORE_diagnostic.command
deriving (Json)

type description = {
  answer   : CORE_identifier.t;
  exercise : CORE_identifier.t;
  jobs     : (CORE_exercise.checkpoint * evaluation_state) list;
} deriving (Json)

}}

{client{
type data = description
}}

let evaluation_of_answer_dependency_kind = "evaluation_of_answer"

let evaluation_of_exercise_dependency_kind = "evaluation_of_exercise"

let new_evaluation_state_of_checkpoint c s d =
  return (Some { d with jobs = update_assoc c s d.jobs })

let create_job checkpoint context submission change_later =
  let observer _ = return () in
  CORE_sandbox.exec [] "echo" observer >>= function
    | `OK (job, _) ->
      let job = ExecutableJob job in
      Lwt.async (fun () ->
        let rec aux k =
          Lwt_unix.sleep 3.
          >> if k = 0 then
              change_later (
                new_evaluation_state_of_checkpoint checkpoint (Evaluated [])
              )
            else (
              change_later (
                new_evaluation_state_of_checkpoint checkpoint (
                  BeingEvaluated (job,
                                  CORE_diagnostic.PushLine (string_of_int k)))
              ) >> aux (k - 1)
            )
        in
        aux 3
      );
      return job
    | `KO e -> (* FIXME *) warn e; assert false

let cancel_job_if_present d cp =
  (* FIXME *)
  return ()

let evaluate change_later exercise answer cps data =
  let evaluate checkpoint =
    cancel_job_if_present data checkpoint
    >> CORE_answer.submission_of_checkpoint answer checkpoint
    >>= function
      | None | Some NoSubmission ->
        return (checkpoint, Unevaluated)
      | Some (HandledSubmission s | NewSubmission s) ->
        lwt c =
          CORE_exercise.context_of_checkpoint exercise checkpoint
        in
        CORE_answer.mark_handled_submission answer checkpoint s
        >> lwt job = create_job checkpoint c s change_later in
        return (checkpoint, BeingEvaluated (job, CORE_diagnostic.Reset))
  in
  Lwt_list.map_p evaluate cps >>= function
    | [] -> return None
    | new_jobs ->
      let jobs =
        List.fold_left
          (fun jobs (k, v) -> update_assoc k v jobs)
          data.jobs
          new_jobs
      in
      return (Some { data with jobs })

let evaluate_all change_later exercise answer data =
  lwt all_cps = CORE_exercise.all_checkpoints exercise in
  evaluate change_later exercise answer all_cps data

include CORE_entity.Make (struct

  type data = description deriving (Json)

  let react change_later deps new_data data =

    match CORE_inmemory_entity.to_list deps with

      | [] ->
        (** Only the state of the evaluation changed. We do not react. *)
        passive change_later deps new_data data

      | _ ->
        (** One of the dependencies changed:

            - If the exercise has changed, we must recheck everything.
            - If the answer has changed,
              we only recheck the modified submissions. *)
        (
          CORE_answer.make data.answer >>>= fun answer ->
          CORE_exercise.make data.exercise >>>= fun exercise ->
          lwt cps =
            match dependency deps evaluation_of_exercise_dependency_kind [] with
              | Some _ -> (** The exercise changed. *)
                Ocsigen_messages.errlog "Exercise";
                CORE_exercise.all_checkpoints exercise
              | None -> (** Only the answer changed. *)
                Ocsigen_messages.errlog "Only answer";
                CORE_answer.checkpoints_of_new_submissions answer
          in
          lwt result = evaluate change_later exercise answer cps data in
          return (`OK result)
        ) >>= function
          | `OK e -> return e
          | `KO e -> warn e; return None (* FIXME *)

end)

let who = "core.evaluation <here@hackojo.org>"

let identifier_of_answer_evaluation answer_id =
  let path = path_of_identifier answer_id in
  let path = concat path (from_strings ["evaluation"]) in
  (** We ensure the existence of [path]. *)
  CORE_vfs.create who path
  >>= function _ -> return (identifier_of_path path)

let initial_evaluation exercise answer = {
  exercise = CORE_exercise.identifier exercise;
  answer = CORE_answer.identifier answer;
  jobs = [];
}

let evaluation_dependencies exercise answer =
  let answer = CORE_answer.identifier answer
  and exercise = CORE_exercise.identifier exercise in
  CORE_inmemory_entity.of_list [
    (evaluation_of_answer_dependency_kind,   [ ([], answer) ]);
    (evaluation_of_exercise_dependency_kind, [ ([], exercise) ])
  ]

let activate exo answer evaluation =
  (* FIXME: define a rec_change in CORE_entity? *)
  change ~immediate:true evaluation (
    evaluate_all (change evaluation) exo answer
  )

let create id exo answer =
  let init = (
    initial_evaluation exo answer,
    evaluation_dependencies exo answer,
    CORE_property.empty,
    []
  ) in
  make ~init id >>= function
    | `OK e -> return (`OK e)
    | r -> return r

let evaluation_of_exercise_from_authors exo answer =
  let answer_id = CORE_answer.identifier answer in
  lwt id = identifier_of_answer_evaluation answer_id in
  make id >>= function
    | `OK e -> return (`OK e)
    | `KO (`UndefinedEntity _) -> create id exo answer
    | `KO e -> return (`KO e)

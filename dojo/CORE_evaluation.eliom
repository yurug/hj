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
  | ImmediateEvaluation
deriving (Json)

let string_of_job = function
  | ExecutableJob job -> CORE_sandbox.string_of_job job
  | ImmediateEvaluation -> "immediate"

type submission = string deriving (Json)

type evaluation_state =
  | Unevaluated
  | Evaluated of CORE_context.score * CORE_diagnostic.command * CORE_context.t
  | BeingEvaluated of job * CORE_diagnostic.command * CORE_context.t
deriving (Json)

let string_of_evaluation_state = function
  | Unevaluated ->
    "Not evaluated"
  | Evaluated (score, _, _) ->
    Printf.sprintf "Evaluated with score %s"
      (CORE_context.string_of_score score)
  | BeingEvaluated (job, _, _) ->
    Printf.sprintf "Being evaluated by job %s" (string_of_job job)

type description = {
  answer   : CORE_identifier.t;
  exercise : CORE_identifier.t;
  jobs     : (CORE_exercise.checkpoint * evaluation_state) list;
} deriving (Json)

}}

{client{
type data = description
}}

type public_change =
  | FlushDiagnosticCommandsOfCheckpoint of CORE_exercise.checkpoint
  | NewEvaluationState of CORE_exercise.checkpoint * evaluation_state

let evaluation_of_answer_dependency_kind = "evaluation_of_answer"

let evaluation_of_exercise_dependency_kind = "evaluation_of_exercise"

let new_evaluation_state_of_checkpoint c s d =
  let s =
    match COMMON_pervasives.opt_assoc c d.jobs with
      | None -> s
      | Some s' ->
        match s', s with
          (* FIXME: Some of these cases do not make sense. *)
          | BeingEvaluated (_, cmd, c), BeingEvaluated (job, cmd', c')
            when c = c' ->
            BeingEvaluated (job, CORE_diagnostic.merge cmd cmd', c)

          | Evaluated (s, cmd, c), BeingEvaluated (_, cmd', c')
            when c = c' ->
            Evaluated (s, CORE_diagnostic.merge cmd cmd', c)

          | BeingEvaluated (job, cmd, c), Evaluated (s, cmd', c')
            when c = c' ->
            Evaluated (s, CORE_diagnostic.merge cmd cmd', c)

          | Evaluated (s, cmd, c), Evaluated (_, cmd', c')
            when c = c' ->
            Evaluated (s, CORE_diagnostic.merge cmd cmd', c)

          | _, s ->
            s
  in
  return { d with jobs = update_assoc c s d.jobs }

let create_job checkpoint context submission change_later =
  let score = ref CORE_context.null_score in
  let seed = CORE_context.make_seed () in
  let change_state s =
    Ocsigen_messages.errlog "Change state";
    change_later (NewEvaluationState (checkpoint, s))
  in
  let message job msg =
    change_state (BeingEvaluated (job, CORE_diagnostic.PushLine msg, context))
  in
  let mark () =
    change_state (Evaluated (!score, CORE_diagnostic.Empty, context))
  in
  let init () =
    change_state Unevaluated
  in
  init ()
  >> let process_stdio job line =
    match CORE_context.marker_io_interpretation seed line with
      | Some s ->
        score := CORE_context.new_score s !score;
        message job "Score!" (* FIXME *)
      | None ->
        message job line
  in
  let observer = CORE_sandbox.(function
    | WriteStdout (job, l) ->
      process_stdio (ExecutableJob job) l
    | WriteStderr (job, l) ->
      (* FIXME: Put that in the private log for teachers. *)
      return ()
    | Exited _ ->
      mark ()
    | _ -> return ()
  ) in
  CORE_context.(
    match get_command context with
      | None ->
        return None (* FIXME: is it sound? *)
      | Some (`ExpectedValues vs) ->
        score := CORE_context.check_expected_values vs submission;
        mark ()
        >> return (Some ImmediateEvaluation)
      | Some (`ExpectedChoices vs) ->
        score := CORE_context.check_expected_choices vs submission;
        mark ()
        >> return (Some ImmediateEvaluation)
      | Some (`Command cmd) ->
        let timeout =
          match get_timeout context with
            | None -> 120. (* FIXME: Make it a parameter. *)
            | Some t -> float_of_int t
        in
        let cmd = CORE_context.substitute_seed seed cmd in
        (CORE_sandbox.exec
           ~limitations:[CORE_sandbox.TimeOut timeout]
           []
           cmd
           observer
        ) >>= function
          | `OK (job, _) ->
            return (Some (ExecutableJob job))
          | `KO e ->
     (* FIXME *) warn e; return None
  )

let cancel_job_if_present job =
  (* FIXME *)
  return ()

let evaluate change_later exercise answer cps data =
  let evaluate checkpoint current_state =
    let run_submission_evaluation c s =
      (CORE_answer.mark_handled_submission answer checkpoint c
       >> create_job checkpoint c s change_later >>= function
         | Some job ->
           Ocsigen_messages.errlog "Job created.";
           return (BeingEvaluated (job, CORE_diagnostic.Empty, c))
         | None ->
           (* FIXME: transmission error. *)
           Ocsigen_messages.errlog "Transmission error";
           return (Evaluated ([], CORE_diagnostic.Empty, c)))
    in

    CORE_answer.submission_of_checkpoint answer checkpoint >>= function
      | None | Some NoSubmission ->
        (** The student has not submitted an answer yet. *)
        return Unevaluated

      | Some (HandledSubmission (s, c')) ->
          (**
            The student's submission has already been processed or
            is in the process of being processed.

            We have two different subcases here depending on the
            evaluation context.
        *)
        begin
          try_lwt
            lwt c = CORE_exercise.context_of_checkpoint exercise checkpoint in
            lwt job, c' = match current_state with
              | Some (Evaluated (_, _, c')) -> return (None, Some c')
              | Some (BeingEvaluated (job, _, c')) -> return (Some job, Some c')
              | _ -> raise_lwt Not_found
            in
            Ocsigen_messages.errlog (
              match c' with
                | None -> "no context"
                | Some c' -> "context " ^ CORE_context.string_of_context c'
            );
            if Some c = c' then (
              Ocsigen_messages.errlog "Same context, do nothing";
              match current_state with
                | None -> assert false
                | Some e -> return e
            ) else (
              Ocsigen_messages.errlog "New context, reeval";
            (** We have already have a score but with a different
                context. It must be recomputed. *)
              cancel_job_if_present job
              >> run_submission_evaluation c s
            )
        end

      | Some (NewSubmission s) ->
        lwt c = CORE_exercise.context_of_checkpoint exercise checkpoint in
        run_submission_evaluation c s

  in
  try_lwt
    lwt jobs = Lwt_list.fold_left_s (fun jobs cp ->
      let e = try Some (List.assoc cp data.jobs) with Not_found -> None in
      lwt e = evaluate cp e in
      return ((cp, e) :: jobs)
    ) [] cps
    in
    Ocsigen_messages.errlog ("Updating jobs of evaluation");
    return (UpdateContent { data with jobs })
  with Not_found ->
    (** We are in the case where a submission is marked as
        handled by a reaction of this evaluation but it
        has not finished reacting. We let it finish
        that reaction. *)
    return NoUpdate


(* let evaluate_all change_later exercise answer data =
  lwt all_cps = CORE_exercise.all_checkpoints exercise in
  evaluate change_later exercise answer all_cps data
*)

include CORE_entity.Make (struct

  type data = description deriving (Json)

  type change = public_change

  let string_of_change = function
    | FlushDiagnosticCommandsOfCheckpoint c ->
      Printf.sprintf "Resetting diagnostics of %s." c
    | NewEvaluationState (c, s) ->
      Printf.sprintf "New evaluation of %s: %s."
        c
        (string_of_evaluation_state s)

  let react state deps cs (change_later : change -> unit Lwt.t) =

    let flush_diagnostic_commands_of_checkpoint checkpoint content =
      return { content with jobs =
          COMMON_pervasives.map_assoc checkpoint (function
            | Evaluated (s, dcmd, c) ->
              Evaluated (s, CORE_diagnostic.Empty, c)
            | BeingEvaluated (j, dcmd, c) ->
              BeingEvaluated (j, CORE_diagnostic.Empty, c)
            | x -> x
          ) content.jobs }
    in
    let make_change content = function
      | FlushDiagnosticCommandsOfCheckpoint c ->
        flush_diagnostic_commands_of_checkpoint c content
      | NewEvaluationState (c, s) ->
        new_evaluation_state_of_checkpoint c s content
    in
    let changes_from_dependencies content =
      match CORE_inmemory_entity.to_list deps with
      | [] ->
        (** Only the state of the evaluation changed. We do not react. *)
        return NoUpdate

      | ldeps ->
        (** One of the dependencies changed:

            - If the exercise has changed, we may have to recheck everything.
            - If the answer has changed,
              we only recheck the modified submissions. *)
        (
          CORE_answer.make content.answer >>>= fun answer ->
          CORE_exercise.make content.exercise >>>= fun exercise ->
          lwt cps =
            match dependency deps evaluation_of_exercise_dependency_kind [] with
              | Some _ -> (** The exercise changed. *)
                CORE_exercise.all_checkpoints exercise
              | None -> (** Only the answer changed. *)
                CORE_answer.checkpoints_of_new_submissions answer
          in
          lwt change = evaluate change_later exercise answer cps content in
          return (`OK change)
        ) >>= function
          | `OK e ->
            return e
          | `KO e ->
            warn e; return NoUpdate
    in
    lwt content = Lwt_list.fold_left_s make_change (content state) cs in
    changes_from_dependencies content

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

(* let activate exo answer evaluation =
  (* FIXME: define a rec_change in CORE_entity? *)
  change ~immediate:true evaluation (
    evaluate_all (fun c -> Lwt.async (fun () -> change evaluation c)) exo answer
  ) *)

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

let flush_diagnostic_commands_of_checkpoint evaluation checkpoint =
  change evaluation (FlushDiagnosticCommandsOfCheckpoint checkpoint)

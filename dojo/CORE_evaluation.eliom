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
  | Evaluated of CORE_context.score * CORE_diagnostic.command * CORE_context.t
  | BeingEvaluated of job * CORE_diagnostic.command * CORE_context.t
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

          | _, Unevaluated ->
            s'

          | Unevaluated, _ ->
            s

          | _, s ->
            s
  in
  let ss = match s with
    | BeingEvaluated _ -> "en cours"
    | Evaluated _ -> "done"
    | _ -> "not done"
  in
  Ocsigen_messages.errlog (Printf.sprintf "New eval state of cp (%s)" ss);
  return (Some { d with jobs = update_assoc c s d.jobs })

let create_job checkpoint context submission change_later =
  let score = ref CORE_context.null_score in
  let seed = CORE_context.make_seed () in
  let change_state s =
    return (change_later (new_evaluation_state_of_checkpoint checkpoint s))
  in
  let message job msg =
    change_state (BeingEvaluated (job, CORE_diagnostic.PushLine msg, context))
  in
  let mark () =
    change_state (Evaluated (!score, CORE_diagnostic.Empty, context))
  in
  let process_stdio job line =
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
      Ocsigen_messages.errlog "Mark";
      mark ()
    | _ -> return ()
  ) in
  CORE_context.(
    match get_command context with
      | None ->
        return None (* FIXME: is it sound? *)
      | Some cmd ->
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
    (** *)
    let run_submission_evaluation c s =
      (CORE_answer.mark_handled_submission answer checkpoint s c
       >> create_job checkpoint c s change_later >>= function
         | Some job ->
           return (BeingEvaluated (job, CORE_diagnostic.Empty, c))
         | None ->
           (* FIXME: transmit error. *)
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
        lwt c = CORE_exercise.context_of_checkpoint exercise checkpoint in
        let job, c' = match current_state with
          | Some (Evaluated (_, _, c')) -> (None, Some c')
          | Some (BeingEvaluated (job, _, c')) -> (Some job, Some c')
          | _ -> (None, None)
        in
        if Some c = c' then
          match current_state with
            | None -> assert false
            | Some e -> return e
        else (
          (** We have already have a score but with a different
              context. It must be recomputed. *)
          cancel_job_if_present job
          >> run_submission_evaluation c s
        )

      | Some (NewSubmission s) ->
        lwt c = CORE_exercise.context_of_checkpoint exercise checkpoint in
        run_submission_evaluation c s

  in
  lwt jobs = Lwt_list.fold_left_s (fun jobs cp ->
    let e = try Some (List.assoc cp data.jobs) with Not_found -> None in
    lwt e = evaluate cp e in
    return ((cp, e) :: jobs)
  ) [] cps
  in
  return (Some { data with jobs })

let evaluate_all change_later exercise answer data =
  lwt all_cps = CORE_exercise.all_checkpoints exercise in
  evaluate change_later exercise answer all_cps data

include CORE_entity.Make (struct

  type data = description deriving (Json)

  let react this change_later deps new_data data =

    match CORE_inmemory_entity.to_list deps with

      | [] ->
        (** Only the state of the evaluation changed. We do not react. *)
        passive this change_later deps new_data data

      | ldeps ->
        (** One of the dependencies changed:

            - If the exercise has changed, we may have to recheck everything.
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
          | `OK e ->
            return e
          | `KO e ->
            warn e; return None (* FIXME *)

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
    evaluate_all (fun c -> Lwt.async (fun () -> change evaluation c)) exo answer
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

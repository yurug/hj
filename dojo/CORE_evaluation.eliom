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

type timestamp = float deriving (Json)

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

  | Evaluated of
      CORE_context.score
    * CORE_context.submission
    * CORE_diagnostic.command
    * CORE_context.t

  | BeingEvaluated of
      job
    * timestamp
    * CORE_context.submission
    * CORE_diagnostic.command
    * CORE_context.t

deriving (Json)

type description = {
  answer       : CORE_identifier.t;
  exercise     : CORE_identifier.t;
  authors      : CORE_identifier.t list;
  jobs         : (CORE_exercise.checkpoint * evaluation_state) list;
  extra_fields : (string * string) list;
} deriving (Json)

}}

let string_of_evaluation_state = function
  | Unevaluated ->
    "Not evaluated"
  | Evaluated (score, _, d, ctx) ->
    Printf.sprintf "Evaluated with score %s (%s)"
      (CORE_context.string_of_score ctx score)
      (CORE_diagnostic.string_of_command d)
  | BeingEvaluated (job, start, _, d, _) -> Unix.(
    Printf.sprintf "Being evaluated by job %s since %s (%s)"
      (string_of_job job) (string_of_date start)
      (CORE_diagnostic.string_of_command d)
  )

{client{
type data = description
}}

let now () = Unix.gettimeofday ()

type public_change =
  | FlushDiagnosticCommandsOfCheckpoint of CORE_exercise.checkpoint
  | NewEvaluationState of CORE_exercise.checkpoint * evaluation_state

let evaluation_of_answer_dependency_kind = "evaluation_of_answer"

let evaluation_of_exercise_dependency_kind = "evaluation_of_exercise"

let new_evaluation_state_of_checkpoint c s d =
  let s_out =
    match COMMON_pervasives.opt_assoc c d.jobs with
      | None -> s
      | Some s' ->
        Ocsigen_messages.errlog (Printf.sprintf "Merging state:\n %s\n  %s\n"
                                   (string_of_evaluation_state s')
                                   (string_of_evaluation_state s));
        match s', s with
          (* FIXME: Some of these cases do not make sense. *)
          | (BeingEvaluated (_, d, s, cmd, c),
             BeingEvaluated (job, d', s', cmd', c'))
            when CORE_context.(
              equivalent_submission s s' && equivalent_context c c' && d = d'
            ) ->
            (** A new intermediate state in an on-going evaluation.
                We merge the diagnostic. *)
            Ocsigen_messages.errlog "Continue...";
            BeingEvaluated (job, d, s, CORE_diagnostic.merge cmd cmd', c)

          | (Evaluated (score, s, cmd, c),
             BeingEvaluated (_, _, s', cmd', c'))
            when CORE_context.(
              equivalent_submission s s' && equivalent_context c c'
            ) ->
            (** The answer and the evaluation did not change. We keep
                the current evaluation. *)
            (* FIXME: We may provide a way for the user to force the
               FIXME: evaluation in that case.*)
            Evaluated (score, s, CORE_diagnostic.merge cmd cmd', c)

          | (BeingEvaluated (job, _, s, cmd, c),
             Evaluated (score, s', cmd', c'))
            when CORE_context.(
              equivalent_submission s s' && equivalent_context c c'
            ) ->
            (** We are done with the evaluation. *)
            Evaluated (score, s', CORE_diagnostic.merge cmd cmd', c)

          | (Evaluated (_, s, cmd, c), Evaluated (score, s', cmd', c'))
            when CORE_context.(
              equivalent_submission s s' && equivalent_context c c'
            ) ->
            (** This is the case of an instantaneous evaluation. *)
            Evaluated (score, s', CORE_diagnostic.merge cmd cmd', c)

          | _, s ->
            (* FIXME: We should integrate here a notion of inherited score:
               FIXME: for instance, if an answer has not changed, the
               FIXME: grade assigned by a master should be inherited. *)
            Ocsigen_messages.errlog "Overwriting...";
            s
  in
  Ocsigen_messages.errlog (Printf.sprintf "Push %s, Get: %s\n"
                             (string_of_evaluation_state s)
                             (string_of_evaluation_state s_out));
  return { d with jobs = update_assoc c s_out d.jobs }

let create_job
    exercise answer_id checkpoint context submission change_later authors
    =
  let exo_id = CORE_exercise.identifier exercise in
  let score = ref CORE_context.null_score in
  let seed = CORE_context.make_seed () in
  let change_state s = change_later (NewEvaluationState (checkpoint, s)) in
  let message job msg = change_state (
    BeingEvaluated (job, now (), submission,
                    CORE_diagnostic.PushLine msg, context)
  )
  in
  let mark () =
    let send_notification_to_master () = CORE_context.(
      match (get_master_grade context, get_master_focus context) with
        | None, _ | _, None -> return ()
        | Some _, Some groups ->
          (** Look for authors that are teachers and that are in the same
              groups as the answer's authors. *)
          lwt authors_groups =
            Lwt_list.(filter_s (fun g ->
              exists_s
                (fun a -> CORE_user.has_property a (CORE_property.atom g))
                authors
            )) groups
          in
          lwt masters =
            lwt all_answers = CORE_answer.answers_of_exercise exercise in
            let all_authors = List.(flatten (fst (split all_answers))) in
            Lwt_list.(filter_s (fun a ->
              CORE_user.(make a >>= function
                | `KO _ -> return false (* FIXME: handle error. *)
                | `OK a ->
                  is_teacher a >>= function
                    | true ->
                      exists_s
                        (fun g -> has_property a (CORE_property.atom g))
                        authors_groups
                    | false -> return false
              )) all_authors)
          in
          Lwt_list.iter_s (fun a ->
            CORE_user.(make a >>= function
              | `OK a -> send a (CORE_message.evaluation_needed exo_id)
              | `KO _ -> return () (* FIXME: should never happen. *)
          )) masters
    )
    in
    send_notification_to_master ()
    >> change_state (
      if !score = CORE_context.null_score then
        Unevaluated
      else
        Evaluated (!score, submission, CORE_diagnostic.Empty, context)
    )
  in
  let init () = change_state Unevaluated in
  init () >>
    let process_stdio job line =
      match CORE_context.marker_io_interpretation seed line with
        | Some s ->
          score := CORE_context.new_score s !score;
          return ()
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
    (* FIXME: All the following should be moved to CORE_context. *)
    match get_command context with
      | None ->
        return None (* FIXME: is it sound? *)
      | Some (`ChooseProperty cs) ->
        lwt status = CORE_context.set_chosen_property authors submission cs in
        score := status;
        mark ()
        >>= fun _ -> return (Some ImmediateEvaluation)
      | Some (`ExpectedValues vs) ->
        score := CORE_context.check_expected_values vs submission;
        mark ()
        >>= fun _ -> return (Some ImmediateEvaluation)
      | Some (`ExpectedChoices vs) ->
        score := CORE_context.check_expected_choices vs submission;
        mark ()
        >>= fun _ -> return (Some ImmediateEvaluation)
      | Some (`Command cmd) ->
        let timeout =
          match get_timeout context with
            | None -> 120. (* FIXME: Make it a parameter. *)
            | Some t -> float_of_int t
        in
        let cmd = CORE_context.substitute_seed seed cmd in
        let submissions_files =
          List.map
            (CORE_standard_identifiers.source_filename answer_id)
            (CORE_context.submission_files submission)
        in
        let files =
          List.map
            (CORE_standard_identifiers.source_filename exo_id)
            (get_sources context)
        in
        (CORE_sandbox.exec
           ~limitations:[CORE_sandbox.TimeOut timeout]
           (submissions_files @ files)
           cmd
           observer
        ) >>= function
          | `OK (job, _) ->
            return (Some (ExecutableJob job))
          | `KO e -> (* FIXME *) warn e; return None
  )

let cancel_job_if_present job =
  (* FIXME *)
  return ()

let evaluate change_later exercise answer cps data authors =
  let answer_id = CORE_answer.identifier answer in
  let authors_ids = List.map CORE_user.identifier authors in
  let evaluate idx checkpoint current_state =
    let rec run_submission_evaluation retry c s =
      create_job exercise answer_id checkpoint c s change_later authors
      >>= function
        | Some job ->
          Ocsigen_messages.errlog "Executable job created.";
          return (BeingEvaluated (job, now (), s, CORE_diagnostic.Empty, c))
        | None ->
          if retry = 0 then (
            Ocsigen_messages.errlog "Error during evaluation.";
            return Unevaluated
          ) else
            Lwt_unix.sleep 1.
            >> run_submission_evaluation (pred retry) c s
    in
    begin CORE_answer.submission_of_checkpoint answer checkpoint >>= function
      | None | Some NoSubmission ->
        return None

      | Some (Submission (_, s)) ->
        return (Some s)
    end >>= function
      | None ->
        (** The student has not submitted an answer yet. *)
        return Unevaluated

      | Some s ->
        begin
          let job, submission, context, too_old =
            match current_state with
              | Evaluated (score, submission, _, context) ->
                None, Some submission, Some context, score = []

              | BeingEvaluated (job, date, submission, _, context) ->
                (* FIXME: Make 1200. a parameter. *)
                Some job, Some submission, Some context,
                (now () -. date > 1200.)

              | Unevaluated ->
                None, None, None, true
          in

          (** Is it a new submission? *)
          let is_new_submission =
            match submission, s with
              | None, _ -> true
              | Some s', s -> not (CORE_context.equivalent_submission s s')
          in

          (** Is it a new context? *)
          CORE_exercise.context_of_checkpoint exercise checkpoint authors_ids
          >>= function
            | None ->
              return Unevaluated

            | Some c ->
              let is_new_context =
                match context, c with
                  | None, _ -> true
                  | Some c', c -> not (CORE_context.equivalent_context c' c)
              in
              if is_new_context || is_new_submission || too_old then
                cancel_job_if_present job
                 >>= fun _ ->
                 (* FIXME: 3 must be a parameter. *)
                 run_submission_evaluation 3 c s
              else
                return current_state
        end
  in
  lwt _, jobs = Lwt_list.fold_left_s (fun (idx, jobs) cp ->
    let e = try List.assoc cp data.jobs with Not_found -> Unevaluated in
    lwt e = evaluate idx cp e in
    return (idx + 1, (cp, e) :: jobs)
  ) (0, []) cps
  in
  Ocsigen_messages.errlog ("Updating jobs of evaluation");
  return (UpdateContent { data with jobs })

include CORE_entity.Make (struct

  type data = description deriving (Json)

  let current_version = "1.0"
  let converters = []

  type change = public_change

  let string_of_change = function
    | FlushDiagnosticCommandsOfCheckpoint c ->
      Printf.sprintf "Resetting diagnostics of %s." c
    | NewEvaluationState (c, s) ->
      Printf.sprintf "New evaluation of %s: %s."
        c
        (string_of_evaluation_state s)

  let react state deps cs (change_later : change -> unit Lwt.t) =

    let content0 = content state in

    let flush_diagnostic_commands_of_checkpoint checkpoint content =
      return { content with jobs =
          COMMON_pervasives.map_assoc checkpoint (function
            | Evaluated (score, s, dcmd, c) ->
              Evaluated (score, s, CORE_diagnostic.Empty, c)
            | BeingEvaluated (j, d, s, dcmd, c) ->
              BeingEvaluated (j, d, s, CORE_diagnostic.Empty, c)
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
        if content0 == content then
          return NoUpdate
        else
          (** Only the state of the evaluation changed. *)
          return (UpdateContent content)

      | ldeps -> (
        CORE_answer.make content.answer >>>= fun answer ->
        CORE_exercise.make content.exercise >>>= fun exercise ->
        lwt authors =
          Lwt_list.fold_left_s (fun authors a ->
            CORE_user.make a >>= function
              | `OK a -> return (a :: authors)
              | `KO _ -> return authors
          ) [] content.authors
        in
        lwt cps = CORE_exercise.all_checkpoints exercise content.authors in
        lwt change = evaluate change_later exercise answer cps content authors
        in
        return (`OK change)
      ) >>= function
          | `OK e -> return e
          | `KO e -> warn e; return NoUpdate
    in
    lwt content = Lwt_list.fold_left_s make_change content0 cs in
    changes_from_dependencies content

end)

let who = "core.evaluation <here@hackojo.org>"

let identifier_of_answer_evaluation answer_id =
  let path = path_of_identifier answer_id in
  let path = concat path (from_strings ["evaluation"]) in
  (** We ensure the existence of [path]. *)
  CORE_vfs.create who path
  >>= function _ -> return (identifier_of_path path)

let initial_evaluation exercise answer authors = {
  exercise = CORE_exercise.identifier exercise;
  answer = CORE_answer.identifier answer;
  authors;
  jobs = [];
  extra_fields = []
}

let evaluation_dependencies exercise answer =
  let answer = CORE_answer.identifier answer
  and exercise = CORE_exercise.identifier exercise in
  CORE_inmemory_entity.of_list [
    (evaluation_of_answer_dependency_kind,   [ ([], answer) ]);
    (evaluation_of_exercise_dependency_kind, [ ([], exercise) ])
  ]

let create id exo answer authors =
  let init = (
    initial_evaluation exo answer authors,
    evaluation_dependencies exo answer,
    CORE_property.empty,
    []
  ) in
  make ~init id >>= function
    | `OK e -> return (`OK e)
    | r -> return r

let evaluation_of_exercise_from_authors exo answer authors =
  let answer_id = CORE_answer.identifier answer in
  lwt id = identifier_of_answer_evaluation answer_id in
  make id >>= function
    | `OK e -> return (`OK e)
    | `KO (`UndefinedEntity _) -> create id exo answer authors
    | `KO e -> return (`KO e)

let flush_diagnostic_commands_of_checkpoint evaluation checkpoint =
  change evaluation (FlushDiagnosticCommandsOfCheckpoint checkpoint)

let state_of_checkpoint evaluation checkpoint =
  observe evaluation (fun d ->
    return (COMMON_pervasives.opt_assoc checkpoint (content d).jobs)
  )

let new_score criteria grade over evaluation checkpoint =
  observe
    evaluation
    (fun d -> return (COMMON_pervasives.opt_assoc checkpoint (content d).jobs))
  >>= function
    | Some (Evaluated (score, s, c, ctx)) ->
      let score = CORE_context.push_grade criteria grade over score in
      let state = Evaluated (score, s, c, ctx) in
      change evaluation (NewEvaluationState (checkpoint, state))
    | _ ->
      (* FIXME: This should not happen. Manual evaluation can only by
         done on evaluated job. *)
      return ()

let reset evaluation checkpoint =
  change evaluation (NewEvaluationState (checkpoint, Unevaluated))

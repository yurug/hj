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

  | Evaluated of
      CORE_context.score
    * CORE_context.submission
    * CORE_diagnostic.command
    * CORE_context.t

  | BeingEvaluated of
      job
    * CORE_context.submission
    * CORE_diagnostic.command
    * CORE_context.t

deriving (Json)

let string_of_evaluation_state = function
  | Unevaluated ->
    "Not evaluated"
  | Evaluated (score, _, _, _) ->
    Printf.sprintf "Evaluated with score %s"
      (CORE_context.string_of_score score)
  | BeingEvaluated (job, _, _, _) ->
    Printf.sprintf "Being evaluated by job %s" (string_of_job job)

type description = {
  answer   : CORE_identifier.t;
  exercise : CORE_identifier.t;
  authors  : CORE_identifier.t list;
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
          | BeingEvaluated (_, s, cmd, c), BeingEvaluated (job, s', cmd', c')
            when CORE_context.(
              equivalent_submission s s' && equivalent_context c c'
            ) ->
            BeingEvaluated (job, s, CORE_diagnostic.merge cmd cmd', c)

          | Evaluated (score, s, cmd, c), BeingEvaluated (_, s', cmd', c')
            when CORE_context.(
              equivalent_submission s s' && equivalent_context c c'
            ) ->
            Evaluated (score, s, CORE_diagnostic.merge cmd cmd', c)

          | BeingEvaluated (job, s, cmd, c), Evaluated (score, s', cmd', c')
            when CORE_context.(
              equivalent_submission s s' && equivalent_context c c'
            ) ->
            Evaluated (score, s, CORE_diagnostic.merge cmd cmd', c)

          | Evaluated (_, s, cmd, c), Evaluated (score, s', cmd', c')
            when CORE_context.(
              equivalent_submission s s' && equivalent_context c c'
            ) ->
            Evaluated (score, s', CORE_diagnostic.merge cmd cmd', c)

          | _, s ->
            s
  in
  return { d with jobs = update_assoc c s d.jobs }

let create_job
    exo_id answer_id checkpoint context submission change_later authors
    =
  let score = ref CORE_context.null_score in
  let seed = CORE_context.make_seed () in
  let change_state s =
    change_later (NewEvaluationState (checkpoint, s))
  in
  let message job msg =
    change_state (
      BeingEvaluated (job, submission, CORE_diagnostic.PushLine msg, context)
    )
  in
  let mark () =
    change_state (
      Evaluated (!score, submission, CORE_diagnostic.Empty, context)
    )
  in
  let init () =
    change_state Unevaluated
  in
  init ()
  >>
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
    (* FIXME: Should be moved to CORE_context. *)
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
          | `KO e ->
     (* FIXME *) warn e; return None
  )

let cancel_job_if_present job =
  (* FIXME *)
  return ()

let evaluate change_later exercise answer cps data authors =
  let exo_id = CORE_exercise.identifier exercise in
  let answer_id = CORE_answer.identifier answer in
  let authors_ids = List.map CORE_user.identifier authors in
  let evaluate checkpoint current_state =
    let run_submission_evaluation c s =
      create_job exo_id answer_id checkpoint c s change_later authors
      >>= function
        | Some job ->
          Ocsigen_messages.errlog "Executable job created.";
          return (BeingEvaluated (job, s, CORE_diagnostic.Empty, c))
        | None ->
           (* FIXME: transmission error. *)
          Ocsigen_messages.errlog "Transmission error";
          return (Evaluated ([], s, CORE_diagnostic.Empty, c))
    in

    CORE_answer.submission_of_checkpoint answer checkpoint >>= function
      | None | Some NoSubmission ->
        (** The student has not submitted an answer yet. *)
        return Unevaluated

      | Some (Submission s) ->
        begin
          let job, submission, context =
            match current_state with
              | Evaluated (_, submission, _, context) ->
                None, Some submission, Some context

              | BeingEvaluated (job, submission, _, context) ->
                Some job, Some submission, Some context

              | Unevaluated ->
                None, None, None
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
              if is_new_context || is_new_submission then
                cancel_job_if_present job
                >>= fun _ -> run_submission_evaluation c s
              else
                return current_state
        end
  in
  lwt jobs = Lwt_list.fold_left_s (fun jobs cp ->
    let e = try List.assoc cp data.jobs with Not_found -> Unevaluated in
    lwt e = evaluate cp e in
    return ((cp, e) :: jobs)
  ) [] cps
  in
  Ocsigen_messages.errlog ("Updating jobs of evaluation");
  return (UpdateContent { data with jobs })

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

    let content0 = content state in

    let flush_diagnostic_commands_of_checkpoint checkpoint content =
      return { content with jobs =
          COMMON_pervasives.map_assoc checkpoint (function
            | Evaluated (score, s, dcmd, c) ->
              Evaluated (score, s, CORE_diagnostic.Empty, c)
            | BeingEvaluated (j, s, dcmd, c) ->
              BeingEvaluated (j, s, CORE_diagnostic.Empty, c)
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

      | ldeps ->
        (
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

(** -*- tuareg -*- *)

(** Answer entities. *)

open Lwt

open CORE_entity
open CORE_inmemory_entity
open CORE_identifier
open CORE_error_messages
open COMMON_pervasives

type description = {
  current_questions : CORE_exercise.questions;
  submissions : (CORE_exercise.checkpoint * CORE_context.submission_state) list;
  evaluations : (CORE_exercise.checkpoint * CORE_context.evaluation_state) list;
} deriving (Json)

let answer_to_dependency_kind = "answer_to"

let answer_of_dependency_kind = "answer_of"

include CORE_entity.Make (struct

  type data = description deriving (Json)

  let react deps new_a old =
    Ocsigen_messages.errlog "Reacting";

    (** Consider the user version of the answer as the new version. *)

    let a = match new_a with None -> old | Some a -> a in

    lwt changed_exo, new_questions =
      match dependency deps answer_to_dependency_kind [] with
        | None -> return (None, None)
        | Some exo_id ->
          CORE_exercise.make exo_id >>= function
            | `OK exo ->
              (** The answer is only interested in changes
                  of the [questions]. *)
              lwt exo_questions =
                CORE_exercise.(observe exo (fun a -> return (questions a)))
              in
              if exo_questions = a.current_questions then
                return (None, None)
              else
                return (Some exo, Some exo_questions)
            | `KO e -> (* FIXME: Handle this inconsistency error.  *)
              warn e;
              assert false
    in

    (** There are several kinds of events to react to:

        - the exercise had been modified, so the evaluation
          context may have been updated and the evaluation
          must redone on all the submissions ;

        - some part of the submissions had been modified, so this
          part must be evaluated again. *)

    lwt checkpoints_to_be_rechecked =
      match changed_exo with
        | Some exo ->
          Ocsigen_messages.errlog "Exo changed";
          (** The exercise is updated. All its checkpoints are
              potentially impacted. *)
          CORE_exercise.all_checkpoints exo

        | None ->
          (** At least one of the submission is updated. *)
          Ocsigen_messages.errlog "Check for submission changed";
          return List.(fst (split (filter (fun (c, s) ->
            CORE_context.is_new_submission s
          ) a.submissions)))
    in
    let questions =
      match new_questions with
        | None -> a.current_questions
        | Some questions -> questions
    in

    (** Re-evaluate checkpoints. *)

    let evaluate a cp =
      lwt context = CORE_exercise.context_of_checkpoint questions cp in
      let change_evaluation_state_of cp estate =
        assert false
      in
      lwt evaluation =
        CORE_context.evaluate
          (opt_assoc cp a.submissions)
          (opt_assoc cp a.evaluations)
          context
          (change_evaluation_state_of cp) (* FIXME *)
      in
      return { a with evaluations = update_assoc cp evaluation a.evaluations }
    in
    lwt a = Lwt_list.fold_left_s evaluate a checkpoints_to_be_rechecked in
    return (match new_questions with
      | None when new_a = None && checkpoints_to_be_rechecked = [] -> None
      | None -> Some a
      | Some qs -> Some { a with current_questions = qs }
    )

end)

let who = "core.answer <here@hackojo.org>"

let path_of_exercise_answers exo_id =
  let path = path_of_identifier exo_id in
  let path = concat path (from_strings ["answers"]) in
  (** We ensure the existence of [path]. *)
  CORE_vfs.create who path
  >>= function _ -> return path

let answer_of exo author =
  let author_id = CORE_user.identifier author in
  dependency
    (CORE_exercise.dependencies exo)
    answer_of_dependency_kind [author_id]

let assign_answer exo answer author =
  CORE_exercise.push_dependency exo answer_of_dependency_kind
    [SomeEntity author]
    (SomeEntity answer)

let answer_of_exercise_from_authors ?(nojoin = true) exo authors =
  let exo_id = CORE_exercise.identifier exo in
  lwt current_questions =
    CORE_exercise.(observe exo (fun a -> return (questions a)))
  in

  (** Determine what are the current answers of the
      given authors list. *)
  let authors_answers = List.map (answer_of exo) authors in

  (** There are several cases to handle:

      (i) The "standard" case:

      All the authors already have the same answer if they have one.
      We continue by updating it and assign this answer to the authors
      that did not have an answer in the past. This allows for user
      group extension, which is quite common.

      (ii) The "initialization" case:

      None of the authors submitted an answer for the moment. We create
      a fresh answer and assign it to the authors.

      (iii) The "join" case:

      The authors already had submitted answers but they are not the
      same. This means that several groups are trying to merge their
      work. [nojoin] indicates if this is allowed or not. A join
      requires to merge all the answers into one and to reassign
      this answer to all the authors. The authors must be warned
      that the history of the other answers will be lost.

  *)

  let rec discriminate first_answer answers =
    match first_answer, answers with
      | None, []                              -> `Initialization
      | None, x :: xs                         -> discriminate x xs
      | Some a, []                            -> `Standard a
      | Some a, (Some b) :: xs when equal a b -> discriminate first_answer xs
      | Some a, (Some b) :: xs                -> `Join a
      | _, None :: xs                         -> discriminate first_answer xs
  in

  let initialize () =
    let rec aux salt () =
      let data = { submissions = []; evaluations = []; current_questions } in
      let dependencies =
        of_list [(answer_to_dependency_kind, [
          ([],
           CORE_exercise.identifier exo)])]
      in
      let init = (data, dependencies, CORE_property.empty, []) in
      lwt path = path_of_exercise_answers exo_id in
      lwt ids_from_authors = Lwt_list.map_s (fun a ->
        lwt f = CORE_user.firstname a in
        lwt s = CORE_user.surname a in
        return (f ^ "_" ^ s)
      ) authors
      in
      let id = identifier_of_path (
        concat path (from_strings [ String.concat "-" ids_from_authors ^ salt ])
      )
      in
      make ~init id >>= function
        | `OK a                  -> return (`OK a)
        | `KO (`AlreadyExists _) -> aux (salt ^ "_") ()
        | `KO e                  -> warn e; return (`KO e)
    in
    aux "" ()
  in

  (match discriminate None authors_answers with
    | `Initialization -> initialize ()
    | `Standard a -> make a
    (* FIXME: To be implemented. See previous comment. *)
    | `Join a -> assert false
  ) >>>= fun a ->
  Lwt_list.iter_s (assign_answer exo a) authors
  >> return (`OK a)

let submit_file answer checkpoint tmp_filename original_filename =
  ltry (fun lraise ->
    lwt content = COMMON_unix.cat tmp_filename lraise in
    let sfname = Filename.basename original_filename in
    let source = CORE_source.make sfname content in
    let sstate = CORE_context.new_submission sfname in
    update_source answer checkpoint source;
    >> change ~immediate:true answer (fun a ->
      let submissions = update_assoc checkpoint sstate a.submissions in
      return (Some { a with submissions })
    )
  )

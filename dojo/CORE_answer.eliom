(** -*- tuareg -*- *)

(** Answer entities. *)

open Lwt

open CORE_entity
open CORE_inmemory_entity
open CORE_identifier
open CORE_error_messages
open COMMON_pervasives

type submission = string deriving (Json)

(** Relation with other entities:
    - authors    : CORE_user.t list
    - exercise   : CORE_exercise.t
*)

type evaluation_dynamic_state =
  | Unevaluated
  | Evaluated      of CORE_context.score
  | BeingEvaluated of CORE_context.job
deriving (Json)

type evaluation_state = {
  estate        : evaluation_dynamic_state;
  rule          : CORE_context.rule list;
  esubmission   : submission option;
} deriving (Json)

type submission_dynamic_state =
  | New
  | Handled
deriving (Json)

type submission_state = {
  submission    : submission;
  sstate        : submission_dynamic_state;
} deriving (Json)

type description = {
  submissions : (CORE_exercise.checkpoint * submission_state) list;
  evaluations : (CORE_exercise.checkpoint * evaluation_state) list;
} deriving (Json)

let answer_to_dependency_kind = "answer_to"

let answer_of_dependency_kind = "answer_of"



include CORE_entity.Make (struct

  type data = description deriving (Json)

  let react deps a old =
    lwt changed_exo =
      match dependency deps answer_to_dependency_kind [] with
        | None -> return None
        | Some exo_id ->
          CORE_exercise.make exo_id >>= function
            | `OK exo -> return (Some exo)
            | `KO exo -> (* FIXME: Handle this inconsistency error.  *)
              Ocsigen_messages.errlog "Error loading exercise in answer.";
              assert false
    in
    (** There are several kind of events to react to:

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
          match a with
            | None -> return []
            | Some a ->
              (** At least one of the submission is updated. *)
              Ocsigen_messages.errlog "Submission changed";
              return (fst (List.split (List.filter (function
                | (c, { submission; sstate = New }) -> true
                | _ -> false
              ) a.submissions)))
    in
    (** Recheck checkpoints. *)
    match a with
      | None -> return old
      | Some a -> return a


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
      | None, [] -> `Initialization
      | None, x :: xs -> discriminate x xs
      | Some a, [] -> `Standard a
      | Some a, (Some b) :: xs ->
        if CORE_identifier.equal a b then
          discriminate first_answer xs
        else
          `Join a
      | _, None :: xs ->
        discriminate first_answer xs
  in
  (match discriminate None authors_answers with
    | `Initialization ->

      let rec aux salt () =
        let data = { submissions = []; evaluations = [] } in
        let dependencies = of_list [(answer_to_dependency_kind, [([], CORE_exercise.identifier exo)])] in
        let init = (data, dependencies, CORE_property.empty, []) in
        lwt path = path_of_exercise_answers exo_id in
        lwt ids_from_authors = Lwt_list.map_s (fun a ->
          lwt f = CORE_user.firstname a in
          lwt s = CORE_user.surname a in
          return (f ^ "_" ^ s)
        ) authors
        in
        let id = identifier_of_path (
          concat path (from_strings [
            String.concat "_" ids_from_authors ^ salt
          ])
        )
        in
        make ~init id >>= function
          | `OK a -> return (`OK a)
          | `KO (`AlreadyExists _) -> aux (salt ^ "_") ()
          | `KO e -> return (`KO e)
      in
      aux "" ()

    | `Standard a ->
      make a

    | `Join a ->
      (* FIXME: To be implemented. See previous comment. *)
      assert false

  ) >>>= fun a ->
  Lwt_list.iter_s (assign_answer exo a) authors
  >> return (`OK a)

let submit_file answer checkpoint tmp_filename original_filename =
  ltry (fun lraise ->
    lwt content = COMMON_unix.cat tmp_filename lraise in
    let sfname = Filename.basename original_filename in
    let source = CORE_source.make sfname content in
    let sstate = { submission = sfname; sstate = New } in
    change answer (fun a ->
      let submissions = update_assoc checkpoint sstate a.submissions in
      return { a with submissions }
    ) >> update_source answer checkpoint source
  )

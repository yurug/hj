(** -*- tuareg -*- *)

(** Answer entities. *)

open Lwt

open CORE_inmemory_entity
open CORE_entity
open CORE_identifier
open CORE_error_messages
open COMMON_pervasives

type submission_state =
  | NoSubmission
  | Submission of int * CORE_context.submission

deriving (Json)

type description = {
  submissions  : (CORE_exercise.checkpoint * submission_state) list;
  extra_fields : (string * string) list;
} deriving (Json)

let answer_to_dependency_kind = "answer_to"

type public_change =
  | NewSubmissionData of
      CORE_exercise.checkpoint
    * CORE_context.submission
    * CORE_source.t list

include CORE_entity.Make (struct

  type data = description deriving (Json)

  let current_version = "1.0"
  let converters = []

  type change = public_change

  let string_of_change = function
    | NewSubmissionData (c, s, ss) ->
      Printf.sprintf "Submit %s for checkpoint %s (sources:%s)."
        (CORE_context.string_of_submission s)
        (CORE_exercise.string_of_checkpoint c)
        (if ss = [] then
            "no"
         else
            (String.concat " " (List.map CORE_source.filename ss))
        )

  exception InvalidCheckpoint

  let react state deps changes change_later =
    let update_source s =
      CORE_onthedisk_entity.save_source
        (CORE_inmemory_entity.identifier state)
        s
      >> return ()
      (* FIXME: take the resulting boolean into account. *)
    in
    let make_change (ss, source_updates) change =
      match change with
      | NewSubmissionData (c, s, sources) ->
        Lwt_list.iter_s update_source sources
        >> (
          let count =
            try
              match List.assoc c ss with
                | NoSubmission -> 0
                | Submission (c, _) -> c
            with Not_found -> 0
          in
          let ss = update_assoc c (Submission (count + 1, s)) ss in
          let sources = List.map CORE_source.filename sources in
          return (ss, sources @ source_updates)
        )
    in
    try
      lwt submissions, source_updates =
        Lwt_list.fold_left_s make_change
          ((content state).submissions, []) changes
      in
      return (state_changes [
        if (submissions == (content state).submissions) then
          NoUpdate
        else UpdateContent { (content state) with submissions };
        if source_updates = [] then
          NoUpdate
        else
          UpdateSources source_updates
      ])
    with InvalidCheckpoint ->
      return NoUpdate

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
  CORE_exercise.observe exo (fun s ->
    return (dependency
              (dependencies s)
              CORE_exercise.answer_of_dependency_kind [author_id])
  )

let assign_answer exo answer author =
  CORE_exercise.change exo
    (CORE_exercise.NewAnswer (
        [CORE_user.identifier author], (identifier answer)
     ))

let answers_of_exercise exo =
  CORE_exercise.observe exo (fun d ->
    return (
      CORE_inmemory_entity.all_dependencies
        (dependencies d)
        CORE_exercise.answer_of_dependency_kind
    )
  )

let answer_of_exercise_from_authors ?(nojoin = true) exo authors =
  let exo_id = CORE_exercise.identifier exo in

  (** Determine what are the current answers of the
      given authors list. *)
  lwt authors_answers = Lwt_list.map_s (answer_of exo) authors in

  List.iter2 (fun a author ->
    Ocsigen_messages.errlog (
      Printf.sprintf "Found answer %s for user %s"
        (match a with None -> "none" | Some a -> string_of_identifier a)
        (string_of_identifier (CORE_user.identifier author)))
  ) authors_answers authors;

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
    let data = { submissions = []; extra_fields = [] } in
    let dependencies =
      of_list [(answer_to_dependency_kind, [
        ([], CORE_exercise.identifier exo)])]
    in
    let init = (data, dependencies, CORE_property.empty, []) in
    lwt path = path_of_exercise_answers exo_id in
    lwt ids_from_authors = Lwt_list.map_s (fun a ->
      lwt l = CORE_user.login a in
      lwt f = CORE_user.firstname a in
      lwt s = CORE_user.surname a in
      let canon x = Str.(global_replace (regexp " \\|/") "" x) in
      return (canon (l ^ "_" ^ f ^ "_" ^ s))
    ) authors
    in
    let id = identifier_of_path (
      concat path (from_strings [ String.concat "-" ids_from_authors ])
      )
    in
    make ~init id >>= function
      | `OK a                  ->
        Lwt_list.iter_s (assign_answer exo a) authors
        >> return (`OK a)
      | `KO (`AlreadyExists _) -> make id
      | `KO e                  -> warn e; return (`KO e)
  in

  (match discriminate None authors_answers with
    | `Initialization -> initialize ()
    | `Standard a -> make a
    (* FIXME: To be implemented. See previous comment. *)
    | `Join a -> assert false
  ) >>>= fun a -> return (`OK a)

let submit answer checkpoint submission =
  change answer (NewSubmissionData (checkpoint, submission, []))

let submit_file answer checkpoint tmp_filename filename =
  ltry (fun lraise ->
    lwt content = COMMON_unix.cat tmp_filename lraise in
    let source = CORE_source.make filename content in
    change answer (
      NewSubmissionData (checkpoint,
                         CORE_context.new_submitted_file filename content,
                         [source])
  ))

let submit_answer_values answer checkpoint values =
  ltry (fun lraise ->
    change answer (
      NewSubmissionData (checkpoint,
                         CORE_context.new_submitted_values values,
                         [])
    )
  )

let submit_answer_choices answer checkpoint choices =
  ltry (fun lraise ->
    change answer (
      NewSubmissionData (checkpoint,
                         CORE_context.new_submitted_choices choices,
                         [])
    )
  )

let submit_property_choice answer checkpoint choice =
  ltry (fun lraise ->
    change answer (
      NewSubmissionData (checkpoint,
                         CORE_context.new_property_choice choice,
                         [])
    )
  )

let submission_of_checkpoint answer cp =
  observe answer (fun a -> return (opt_assoc cp (content a).submissions))

let submission_from_checkpoint_index answer idx =
  let cp_as_int s =
    try
      Scanf.sscanf s "_%d" (fun x -> x)
    with _ -> max_int
  in
  observe answer (fun a ->
    let ss = (content a).submissions in
    let ss = List.map (fun (cp, s) -> cp_as_int cp, s) ss in
    let ss = List.sort (fun (c1, _) (c2, _) -> Pervasives.compare c1 c2) ss in
    try_lwt
      match (snd (List.nth ss idx)) with
        | NoSubmission -> return None
        | Submission (_, s) -> return (Some s)
    with _ -> return None)

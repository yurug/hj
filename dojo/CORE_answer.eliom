(** -*- tuareg -*- *)

(** Answer entities. *)

open Lwt

open CORE_entity
open CORE_inmemory_entity
open CORE_identifier
open CORE_error_messages
open COMMON_pervasives

type submission_state =
  | NoSubmission
  | NewSubmission of CORE_context.submission
  | HandledSubmission of CORE_context.submission * CORE_context.t
deriving (Json)

type description = {
  submissions : (CORE_exercise.checkpoint * submission_state) list;
} deriving (Json)

let answer_to_dependency_kind = "answer_to"

let answer_of_dependency_kind = "answer_of"

include CORE_entity.Make (struct

  type data = description deriving (Json)

  let react this change_later deps new_data data =
    match new_data with
      | None -> return None
      | Some d ->
        if d = data then return None else (
          Ocsigen_messages.errlog "Answer reacts!";
          return (Some d)
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
      let data = { submissions = [] } in
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

let submit_file answer checkpoint tmp_filename filename =
  ltry (fun lraise ->
    lwt content = COMMON_unix.cat tmp_filename lraise in
    let source = CORE_source.make filename content in
    let sstate = NewSubmission (CORE_context.new_submission filename) in
    update_source answer checkpoint source;
    >> change ~immediate:true answer (fun a ->
      let submissions = update_assoc checkpoint sstate a.submissions in
      return (Some { submissions })
    )
  )

let submission_of_checkpoint answer cp =
  observe answer (fun a -> return (opt_assoc cp a.submissions))

let checkpoints_of_new_submissions answer =
  observe answer (fun a ->
    return (fst (List.(split (filter (function
      | (_, NewSubmission _) -> true
      | _ -> false
    ) a.submissions))))
  )

let mark_handled_submission answer cp s c =
  change answer (fun a -> return (Some {
    submissions = update_assoc cp (HandledSubmission (s, c)) a.submissions
  }))

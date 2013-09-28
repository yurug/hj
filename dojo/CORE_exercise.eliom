(** -*- tuareg -*- *)

(** Exercise entities. *)

open Lwt
open Eliom_parameter

open CORE_entity
open CORE_standard_identifiers
open CORE_error_messages
module C = CORE_description_CST
open COMMON_pervasives

{shared{

open CORE_identifier

type composer = Par | Seq deriving (Json)

type questions =
  | Compose           of composer * questions list
  | QuestionReference of CORE_question.reference * CORE_entity.timestamp
 deriving (Json)

type assignment_kind = [ `Must | `Should | `Can | `Cannot ] deriving (Json)

type description = {
  assignment_rules : (assignment_kind * CORE_property.rule list) list;
  questions        : questions;
} deriving (Json)

}}

let timestamp_of_question questions rkey =
  let rec aux = function
    | Compose (_, qs) -> List.flatten (List.map aux qs)
    | QuestionReference (r, ts) when r = rkey -> [ ts ]
    | QuestionReference _ -> []
  in
  aux questions

{client{
type data = description
}}

type patch = C.position * C.position * string

exception Error of [ `UndefinedEntity of CORE_identifier.t
                   | `AlreadyExists   of CORE_identifier.path
                   | `SystemError     of string
                   | `NeedPatch       of patch]

include CORE_entity.Make (struct

  type data = description deriving (Json)

  let react = passive

end)

let question_id_from_user_string sid =
  let id = path_of_string sid in
  identifier_of_path (concat questions_path id)

let with_local_error e = raise (Error e)

let collect_on_questions cst f =
  let rec aux = function
    | C.Compose (_, qs) ->
      lwt nqs = Lwt_list.map_s aux qs in
      return (List.flatten nqs)
    | C.Single (C.Question (id, def)) ->
      let id = question_id_from_user_string id.C.node in
      f id def
  in
  aux cst

let new_inline_questions e cst =
  Ocsigen_messages.errlog ("Searching for new inline definitions");
  collect_on_questions cst (fun id def ->
    CORE_question.make id >>= function
      | `OK q ->
        (** We enforce the fact that the entity [e] depends on [q]. *)
        push_dependency e "questions" [] (SomeEntity q);
        return []
      | `KO e ->
        match def with
          | None ->
            with_local_error e
          | Some def ->
            return [(id, def)]
  )

let outdated_questions e cst =
  Ocsigen_messages.errlog ("Searching for outdated definitions");
  collect_on_questions cst (fun id def ->
    !!> begin fun () -> match def with
      | Some def ->
        let sdef = def.C.statement.C.node in
        CORE_question.make id >>>= fun q ->
        let rid = CORE_question.refer_to q in
        (** There already is a question [q] named [id]. *)
        lwt statement = CORE_question.statement q in
        if statement <> sdef then
            (** There is a conflict between the two definitions of
                the question statement. *)
            observe e (fun data ->
              if List.for_all (fun x -> x >= CORE_question.timestamp q)
                (timestamp_of_question data.questions rid) then (
                Ocsigen_messages.errlog "Exercise is newer!";
                return (`OK [])
              ) else (** The definition has a new statement. Update the
                         inline definition in the exercise. *)
                let patch =
                  C.(def.statement.start, def.statement.stop,
                     "{" ^ statement ^ "}")
                in (
                  Ocsigen_messages.errlog "Question is newer!";
                  return (`OK [patch])
                )
            )
         else return (`OK [])

      | None -> return (`OK [])

   end with_local_error)

let questions_from_cst e cst =
  let composer_from_cst = function
    | C.Par -> Par
    | C.Seq -> Seq
  in
  let rec aux = function
    | C.Compose (c, qs) ->
      lwt qs = Lwt_list.map_s aux qs in
      return (Compose (composer_from_cst c, qs))

    | C.Single (C.Question (id, def)) ->
      let id = question_id_from_user_string id.C.node in
      CORE_question.make id >>= function
        | `OK q -> (match def with
            | None -> return ()
            | Some def ->
              (** At this point, the inline definition is necessarily new. *)
              !!> (fun () ->
                CORE_question.change_from_user_description q def
              ) with_local_error
        ) >>= fun _ ->
          return (QuestionReference (CORE_question.refer_to q,
                                     CORE_question.timestamp q))
        | `KO e -> with_local_error e
  in
  aux cst

let (raw_user_description_filename,
     raw_user_description_source,
     raw_user_description_retrieve)
    = source "description.txt"

{client{
let raw_user_description id = %raw_user_description_retrieve id
}}

let initial_source_filenames = [
  raw_user_description_filename
]

(** Take an exercise [x] and a user description [cr] and produce a
    change on [x] to be up-to-date with respect to [cr].

    In meantime, [cr] may contain outdated information about the
    questions in which case we have to produce patches to apply to the
    user description [cr].

    Finally, if [cr] contains new question definitions, we generate
    requests for the user to confirm their creation.
*)
let change_from_user_description x cr =
  try_lwt
    let cst = C.data cr in
    new_inline_questions x cst >>= function
      | [] ->
        (outdated_questions x cst >>= function
          | [] ->
            Ocsigen_messages.errlog ("Pull done, let us push.");
            lwt questions = questions_from_cst x cst in
            lwt changed = observe x (fun data -> return (data.questions <> questions)) in
              (if changed then
                let data = { assignment_rules = []; questions; } in
                lwt source = raw_user_description_source x in
                CORE_source.set_content source (C.raw cr);
                change x (fun data_now -> return data)
               else return ())
              >> return (`OK [])
          | p :: _ ->
            Ocsigen_messages.errlog ("Patch needed");
            return (`KO (`NeedPatch p))
        )
      | new_inline_questions ->
        Ocsigen_messages.errlog ("New inline definitions");
        return (`OK new_inline_questions)
  with Error e ->
    return (`KO e)

let assignment_rule e k =
  observe e (fun c -> return (
    try
      CORE_property.conjs (List.assoc k c.assignment_rules)
    with Not_found -> CORE_property.True
  ))

let exercise_id username =
  identifier_of_path (
    concat exercises_path (CORE_identifier.make [label username])
  )

let create_service ok_page ko_page =
  Eliom_registration.Redirection.register_service
    ~path:["create_exercise"]
    ~get_params:Eliom_parameter.(suffix (list "id" (string "label")))
    (fun id () ->
      try_lwt
        let id = identifier_of_string_list id in
        let assignment_rules = [] in
        let questions = Compose (Seq, []) in
        let init = (
          { assignment_rules; questions },
          CORE_inmemory_entity.empty_dependencies,
          CORE_property.empty,
          initial_source_filenames
        ) in
        make ~init id >>= function
          | `OK e ->
            return (ok_page e)
          | `KO e ->
            return (ko_page (string_of_error e))
      with InvalidLabel _ ->
       return (ko_page (string_of_error (`InvalidLabel (String.concat "/" id))))
    )

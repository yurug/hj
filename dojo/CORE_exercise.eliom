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
  | InlineQuestion    of identifier * string
  | QuestionReference of CORE_question.reference
 deriving (Json)

type assignment_kind = [ `Must | `Should | `Can | `Cannot ] deriving (Json)

type description = {
  assignment_rules : (assignment_kind * CORE_property.rule list) list;
  questions        : questions;
} deriving (Json)

}}

{client{
type data = description
}}


include CORE_entity.Make (struct

  type data = description deriving (Json)

  let react = passive

end)


type patch = C.position * C.position * string

exception Error of [ `UndefinedEntity of CORE_identifier.t
                   | `AlreadyExists   of CORE_identifier.path
                   | `SystemError     of string]

let questions_from_cst e c =
  let inline_questions = ref [] in
  let patch_inline_question = ref None in

  let local_error e = raise (Error e) in

  let rec aux = function
  | C.Compose (c, qs) ->
    lwt qs = Lwt_list.map_s aux qs in
    return (Compose (composer_from_cst c, qs))

  | C.Single (C.Question (id, def)) ->
    let id = path_of_string id.C.node in
    let id = identifier_of_path (concat questions_path id) in
    begin match def with
      | Some def ->
        let sdef = def.C.statement.C.node in
        (CORE_question.make id >>= function
          | `OK q ->
            (** There already is a question [q] named [id]. *)

            (** We enforce the fact that the entity [e] depends on [q]. *)
            push_dependency e "questions" [] (SomeEntity q);

            lwt statement = CORE_question.statement q in
            lwt statement =
              if statement <> sdef then
                (** There is a conflict between the two definitions of
                    the question statement. *)
                lwt is_newer = (!!> (fun () -> newer_than e (SomeEntity q))
                ) local_error
                in
                if is_newer then (
                  (** The inline definition is newer. Let us push the
                      new statement in the question. *)
                  CORE_question.change_from_user_description q sdef >>= function
                    | `OK () -> return sdef
                    | `KO e -> raise (Error e)
                ) else
                  (** The definition has a new statement. Update the
                      inline definition in the exercise. *)
                  let patch =
                    C.(def.statement.start, def.statement.stop,
                       "{" ^ statement ^ "}\n")
                  in (
                    patch_inline_question := Some patch;
                    return statement
                  )
              else
                return statement
            in
            return (QuestionReference (CORE_question.refer_to q))

          | `KO e ->
            (** The question does not exist. We ask the user
                for its creation. We keep the inline definition
                in the AST as this is the only place where it
                exists for now. *)
            inline_questions := (id, sdef) :: !inline_questions;
            return (InlineQuestion (id, sdef))
      )
      | None ->
        CORE_question.make id >>= function
          | `OK q ->
            return (QuestionReference (CORE_question.refer_to q))
          | `KO e ->
            (* FIXME: We could also choose to ask for the creation of an empty
               question. *)
            raise (Error e)
    end
  and composer_from_cst = function
    | C.Par -> Par
    | C.Seq -> Seq
  in
  lwt c = aux c in
  return (!inline_questions, !patch_inline_question, c)

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

let change_from_user_description x cr =
  try_lwt
    lwt inline_definitions, patches, questions =
      questions_from_cst x (C.data cr)
    in
    let data = {
      assignment_rules = [];
      questions;
    }
    in
    lwt source = raw_user_description_source x in
    CORE_source.set_content source (C.raw cr);
    change x (fun data_now -> return data)
    >> return (`OK (inline_definitions, patches))
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

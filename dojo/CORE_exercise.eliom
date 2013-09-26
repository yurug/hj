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
  | Compose  of composer * questions list
  | Question of identifier (* CORE_question.reference *)
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

let questions_from_cst c =
  let online_questions = ref [] in
  let rec aux = function
  | C.Compose (c, qs) ->
    Compose (composer_from_cst c, List.map aux qs)
  | C.Single (C.Question (id, def)) ->
    let id = identifier_of_string id.C.node in
    begin match def with
      | Some def ->
        let sdef = def.C.statement.C.node in
        online_questions := (id, sdef) :: !online_questions;
      | None -> ()
    end;
    Question id
  and composer_from_cst = function
    | C.Par -> Par
    | C.Seq -> Seq
  in
  let c = aux c in
  (!online_questions, c)

include CORE_entity.Make (struct

  type data = description deriving (Json)

  let react = passive

end)

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
  let online_definitions, questions =
    questions_from_cst (C.data cr)
  in
  let data = {
    assignment_rules = [];
    questions;
  }
  in
  lwt source = raw_user_description_source x in
  CORE_source.set_content source (C.raw cr);
  change x (fun data_now -> return data)
  >> return online_definitions

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

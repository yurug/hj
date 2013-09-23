(** -*- tuareg -*- *)

(** Exercise entities. *)

open Lwt
open Eliom_parameter

open CORE_entity
open CORE_identifier
open CORE_standard_identifiers
open CORE_error_messages
module C = CORE_description_CST
open COMMON_pervasives

type composer = Par | Seq deriving (Json)

type questions =
  | Compose  of composer * questions list
  | Question of identifier (* CORE_question.reference *)
 deriving (Json)

type assignment_kind = [ `Must | `Should | `Can | `Cannot ] deriving (Json)

type description = {
  assignment_rules : (assignment_kind * CORE_property.rule list) list;
  questions : questions
} deriving (Json)

let rec questions_from_cst = function
  | C.Compose (c, qs) ->
    Compose (composer_from_cst c, List.map questions_from_cst qs)
  | C.Single (C.Question (id, _)) ->
    Question (identifier_of_string id.C.node)

and composer_from_cst = function
  | C.Par -> Par
  | C.Seq -> Seq

include CORE_entity.Make (struct

  type data = description deriving (Json)

  let react = passive

end)

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
          CORE_property.empty
        ) in
        make ~init id >>= function
          | `OK e ->
            return (ok_page e)
          | `KO e ->
            return (ko_page (string_of_error e))
      with InvalidLabel _ ->
       return (ko_page (string_of_error (`InvalidLabel (String.concat "/" id))))
    )

(** -*- tuareg -*- *)

(** Exercise entities. *)

open Lwt

open CORE_entity
open CORE_identifier
open CORE_error_messages
open COMMON_pervasives

type questions =
  | Seq      of questions list
  | Par      of questions list
  | Question of CORE_question.reference
 deriving (Json)

type assignment_kind = [ `Must | `Should | `Can | `Cannot ] deriving (Json)

type description = {
  assignment_rules : (assignment_kind * CORE_property.rule list) list;
} deriving (Json)

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

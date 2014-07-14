(* -*- tuareg -*- *)

(** There is a global unique entity responsible for the
    assignments of exercises to students.

    When an exercise is created, a rule of assignment must be
    given. It is used to decide at a given date, what are the
    assignments of a given user with a given set of properties.

    This decision procedure is centralized because we do not want
    to determine the assignments in a time proportional to the
    number of exercises. We'd rather construct a global decision
    tree so that this decision is merely proportional to the
    number of assignment rules.
*)

open CORE_entity
open CORE_identifier

type assignment_kind = [ `Must | `Should | `Can | `Cannot ] deriving (Json)

val assignments
  : assignment_kind -> CORE_property.set -> identifier list Lwt.t

val register_rule
  : identifier -> (assignment_kind * CORE_property.rule) list -> unit Lwt.t

(** -*- tuareg -*- *)

(** Exercise entities. *)

include CORE_entity.S

type assignment_kind = [ `Must | `Should | `Can | `Cannot ]

(** [assignment_rule e k] returns the assignment rule of the
    exercise [e] for the assignment kind [k]. *)
val assignment_rule : t -> assignment_kind -> CORE_property.rule Lwt.t

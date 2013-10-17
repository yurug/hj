(** -*- tuareg -*- *)

(** Answer entities. *)

include CORE_entity.S

val answer_of_exercise_from_authors
  : ?nojoin:bool -> CORE_exercise.t -> CORE_user.t list -> [
    `OK of t
  | `KO of [>
           | `UndefinedEntity of CORE_identifier.t
           | `AlreadyExists   of CORE_identifier.path
           | `SystemError     of string
           ]
  ] Lwt.t

val submit_file
  : t -> string -> string -> string -> [
    `OK of unit
  | `KO of [> `SystemError of string ]
  ] Lwt.t

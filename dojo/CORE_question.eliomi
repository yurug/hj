(** -*- tuareg -*- *)

(** Question entities. *)

include CORE_entity.S

{client{
  type reference deriving (Json)
}}

val change_from_user_description
  : t -> CORE_description_CST.question_definition ->
  [ `OK of unit | `KO of 'a ] Lwt.t

val make_blank
  : CORE_identifier.t ->
  [ `OK of t
  | `KO of [>
           | `UndefinedEntity of CORE_identifier.t
           | `AlreadyExists   of CORE_identifier.path
           | `SystemError     of string
           ]
  ] Lwt.t

val statement : t -> string Lwt.t

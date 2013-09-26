(** -*- tuareg -*- *)

(** Question entities. *)

include CORE_entity.S

{client{
  type reference deriving (Json)
}}

val create_from_user_description
  : CORE_identifier.t -> string -> [ `OK of unit | `KO of unit ] Lwt.t

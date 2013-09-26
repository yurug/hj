(** -*- tuareg -*- *)

(** Question entities. *)

open Lwt

open CORE_entity
open CORE_identifier
open CORE_error_messages
open COMMON_pervasives

type description = unit deriving (Json)

include CORE_entity.Make (struct

  type data = description deriving (Json)

  let react = passive

end)

{client{
  type reference = CORE_identifier.t deriving (Json)
}}

let create_from_user_description id cst =
  return (`OK ())

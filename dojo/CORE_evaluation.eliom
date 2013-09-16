(** -*- tuareg -*- *)

(** Evaluation entities. *)

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

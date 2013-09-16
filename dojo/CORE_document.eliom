(* -*- tuareg -*- *)

open Lwt

open CORE_entity
open CORE_identifier
open CORE_error_messages
open COMMON_pervasives

{shared{

module Text = struct

  type t = string list deriving (Json)

  let empty = []

  let add_line d l = l :: d

  let lines : t -> string list = List.rev

end

}}

type description =
  | Text of Text.t
deriving (Json)

include CORE_entity.Make (struct

  type data = description deriving (Json)

  let react = passive

end)

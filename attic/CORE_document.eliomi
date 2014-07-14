(* -*- tuareg -*- *)

(** Document entities. *)

(** A document is a piece of data that can be read and write following
    a grammar that depends on its type. *)

{shared{

module Text : sig
  type t deriving (Json)
  val empty : t
  val add_line : t -> string -> t
  val lines : t -> string list
end

}}

include CORE_entity.S

(* -*- tuareg -*- *)

(** Documents. *)

(** A document is a piece of data that can be read and write following
    a grammar that depends on its type. *)

{shared{

type t

val empty_text : t

val add_line : t -> string -> t

val lines : t -> string list

}}

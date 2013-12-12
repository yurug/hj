(* -*- tuareg -*- *)

(** User messages for errors. *)

{shared{

val string_of_error : [< CORE_errors.all ] -> string

}}

val fatal_error : [< CORE_errors.all ] -> 'a

val warn : [< CORE_errors.all ] -> unit

val bad_assumption : string -> unit

(* -*- tuareg -*- *)

(** User messages for errors. *)

val string_of_error : [< CORE_errors.all ] -> string

val fatal_error : [< CORE_errors.all ] -> 'a

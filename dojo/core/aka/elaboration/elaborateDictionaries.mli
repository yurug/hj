(** This module transforms a program with implicit dictionaries into a
    program with explicit dictionary passing. *)

(** [program p] is [p] with explicit dictionary passing. *)
val program : XAST.program -> XAST.program

(** Alpha-renaming of programs. *)

exception UnboundVariable of Position.position * Name.name
exception OverloadedSymbolCannotBeBound of Position.position * Name.name

(** [program p] enforces the invariant that all the bound names
    are distinct in the whole program. *)
val program : IAST.program -> IAST.program

(** The syntax for explicitly typed ML with type classes. *)
include module type of AST.Make (Types.ExplicitTyping)

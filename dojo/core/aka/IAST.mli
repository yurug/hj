(** The syntax for implicitly typed ML with type classes. *)
include module type of AST.Make (Types.ImplicitTyping)

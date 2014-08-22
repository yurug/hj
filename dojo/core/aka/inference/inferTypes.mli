(** Type elaboration from implicitly typed ML to explicitly typed ML. *)

(** [program p] returns the explicitely typed version of [p] if it
    exists. No exception should escape this function. *)
val program : IAST.program -> XAST.program

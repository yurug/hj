(** Name scopes. *)

(** Program identifiers. *)
type name = Name of string deriving (Json)

(** Data constructor identifiers. *)
type dname = DName of string deriving (Json)

(** Label identifiers. *)
type lname = LName of string deriving (Json)

(** Type identifiers. *)
type tname = TName of string deriving (Json)

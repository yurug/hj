(* -*- tuareg -*- *)

(** This module implements set of properties. *)

(** {1 Property} *)
type t deriving (Json)

(** A property is parameterized by a value.*)
type value =
  | VInt of int
  | VIdentifier of Identifier.t
deriving (Json)

(** [property p vs] returns a property representing [p (vs)].
    If [p] is not in [[a-zA-Z0-9]+], [InvalidPredicateName p] is raised. *)
val property : string -> value list -> t
exception InvalidPredicateName of string

(** {1 Properties} *)

(** A set of property. *)
type properties deriving (Json)

(** The empty properties. *)
val empty : properties

(** [is p s] returns true iff [p] is in [s]. *)
val is : t -> properties -> bool

(** [assign s p] is the properties [s] with the property [p]. *)
val assign : properties -> t -> properties

(** [unassign s p] is the properties [s] without the property [p]. *)
val unassign : properties -> t -> properties

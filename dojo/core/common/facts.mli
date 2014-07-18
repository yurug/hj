(* -*- tuareg -*- *)

(** This module implements a base of facts.

    A fact is a property that holds in the world. A property is the
    instanciation of a predicate over a tuple of values. Values
    include standard literals (integer, string, float, ...) as well as
    identifiers and timestamps.

    The set of facts can only grow: there is no way to revert the
    assertion of a fact (except of course if you have supercow
    powers).

*)

(** {1 Property} *)
type t deriving (Json)

(** *)

(** A property is parameterized by a value. *)
type value =
  | VInt of int
  | VIdentifier of Identifier.t
  | VTimestamp of Timestamp.t
  |
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

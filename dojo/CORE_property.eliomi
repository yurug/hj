(* -*- tuareg -*- *)

(** This module implements set of properties. *)

{shared{

(** A property. *)
type t deriving (Json)

}}

(** {1 Atoms} *)

(** [valid_atom s] returns [true] if [s] is a valid atom
    string representation. *)
val valid_atom : string -> bool

(** [atom s] returns an atomic property, assuming [valid_atom s]. *)
val atom : string -> t

(** {1 Sets of properties} *)

(** A set of property. *)
type set deriving (Json)

(** The empty set. *)
val empty : set

(** [is p s] returns true iff [p] is in [s]. *)
val is : t -> set -> bool

(** [assign s p] is the set [s] with the property [p]. *)
val assign : set -> t -> set

(** {1 Rules}

    Propositional formulas over properties.

*)
{shared{
type binop = And | Or deriving (Json)

type unop = Not deriving (Json)

type rule =
  | True
  | Is    of t
  | BinOp of binop * rule * rule
  | UnOp  of unop * rule
deriving (Json)
}}

(** [conjs rs] returns the conjunction of rules [rs]. *)
val conjs : rule list -> rule

(** [rule_of_string s] parses [s] as a formula expressed as an (S-exp). *)
val rule_of_string : string -> rule

(** [rule_to_string r] prints [r] as an (S-exp). *)
val rule_to_string : rule -> string

(** [evaluate s r] returns true if the formula [r] is valid under [s]. *)
val evaluate : set -> rule -> bool

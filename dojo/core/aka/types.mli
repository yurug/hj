(** The syntax for types and type annotations. *)

open Positions
open Name

(** The types are first-order terms. *)
type t =
  | TyVar        of position * tname
  | TyApp        of position * tname * t list

(** Type schemes. *)
type scheme = TyScheme of tname list * class_predicates * t

and class_predicate = ClassPredicate of tname * tname

and class_predicates = class_predicate list

type instantiation_kind =
  | TypeApplication of t list
  | LeftImplicit

(** The following module type specifies the differences
    between the variants of ML we provide in {!IAST} and
    {!XAST}. *)
module type TypingSyntax = sig

  (** The amount of type annotations on bindings differs. *)
  type binding

  val binding
    : Lexing.position -> name -> t option -> binding

  val destruct_binding
    : binding -> name * t option

  (** Type applications are either left implicit or
      explicitly given. *)
  type instantiation

  val instantiation
    : Lexing.position -> instantiation_kind -> instantiation

  val destruct_instantiation_as_type_applications
    : instantiation -> t list option

  val implicit : bool

end

(** The typing syntax for implicitly typed ML. *)
module ImplicitTyping : TypingSyntax
  with type binding = name * t option
  and type instantiation = t option

(** The typing syntax for explicitly typed ML. *)
module ExplicitTyping : TypingSyntax
  with type binding = name * t
  and type instantiation = t list

(** [ntyarrow pos [ity0; ... ityN] oty] returns the type of the shape
    [ity0 -> ... ityN -> oty]. *)
val ntyarrow : position -> t list -> t -> t

(** [destruct_ntyarrow ty] returns [([ity0; ... ityN], oty)] if [ty] has
    the shape [ity0 -> ... ityN -> oty]. Otherwise, it returns [([],
    oty)]. *)
val destruct_ntyarrow : t -> t list * t

(** [destruct_tyarrow ty] returns (ity, oty) if [ty = ity -> oty]. *)
val destruct_tyarrow : t -> (t * t) option

(** Types are internally sorted by kinds. *)
type kind =
  | KStar
  | KArrow of kind * kind

(** [kind_of_arity n] returns the kind for type constructors
    of arity [n]. *)
val kind_of_arity : int -> kind

(** [equivalent t1 t2] returns true if [t1] is equivalent to [t2]. *)
val equivalent : t -> t -> bool

(** [substitute s ty] returns [ty] on which the substitution [s]
    has been applied. *)
val substitute : (tname * t) list -> t -> t

(* -*- tuareg -*- *)

(** This module implements a base of facts.

    A fact is a property that holds about the world. A property is the
    instanciation of a predicate over a tuple of values. Values
    include standard values (integer, string, float, ...) as well as
    identifiers and timestamps.

    The set of facts can only grow: there is no way to revert the
    assertion of a fact (except of course if you have supercow
    powers).

*)

open Identifier
open Timestamp

(** {1 Properties} *)

(** A predicate is parameterized by a value of some type in
    the following algebra: *)
type 'a ty =
  | TInt        : int ty
  | TString     : string ty
  | TIdentifier : identifier -> identifier ty
  | TTimestamp  : timestamp -> timestamp ty
  | TPair       : 'a ty * 'b ty -> ('a * 'b) ty

type 'a predicate

(** {1 Facts} *)

type 'a request =
  | Is   : ('a, 'b) pattern -> 'a request
  | Pass : ('a -> bool)     -> 'a request
  | Conj : 'a request list  -> 'a request
  | Disj : 'a request list  -> 'a request

and ('a, 'b) pattern =
  | PTop : 'a predicate                  -> ('a, 'a) pattern
  | PFst : 'c request * ('a, 'b) pattern -> ('a, 'c * 'b) pattern
  | PSnd : ('a, 'b) pattern * 'c request -> ('a, 'b * 'c) pattern

(** [make_predicate pid ty statable_by visible_by] *)
val make_predicate
  : string
  -> 'a ty
  -> identifier request
  -> identifier request
  -> 'a predicate

exception InvalidPredicateName of string

val ask : identifier -> 'a request -> 'a list

val state : identifier -> 'a predicate -> 'a -> unit

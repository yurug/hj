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

open Identifier
open Timestamp

type 'a predicate

and 'a request =
  | Is : 'b predicate * ('a, 'b) pattern -> 'a request

and ('a, 'b) pattern =
  | Hole : ('a, 'a) pattern
  | Fst : 'c * ('a, 'b) pattern -> ('a, 'c * 'b) pattern
  | Snd : ('a, 'b) pattern * 'c -> ('a, 'b * 'c) pattern

type 'a ty =
  | TInt : int ty
  | TString : string ty
  | TFloat : float ty
  | TIdentifier : identifier ty
  | TTimestamp : timestamp ty
  | TPair : 'a ty * 'b ty -> ('a * 'b) ty

val make_predicate
  : string -> identifier request -> identifier request
  -> 'a ty
  -> 'a predicate

val state : identifier -> 'a predicate -> 'a -> unit

type 'a answers =
  | NoAnswer : 'a answers
  | Next : 'a * (unit -> 'a answers) -> 'a answers

val ask : identifier -> 'a request -> 'a answers

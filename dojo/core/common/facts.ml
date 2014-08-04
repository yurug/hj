(* -*- tuareg -*- *)
(** This module implements a base of facts. *)

open Identifier
open Timestamp

type 'a predicate = {
  name        : string;
  ty          : 'a ty;
  visibility  : identifier request;
  permission  : identifier request;
}

and statement =
    Statement : timestamp * identifier * 'a predicate * 'a -> statement

and 'a request =
  | Is : 'b predicate * ('a, 'b) pattern -> 'a request

and ('a, 'b) pattern =
  | Hole : ('a, 'a) pattern
  | Val  :  'b request -> ('a, 'b) pattern
  | Fst  : 'c * ('a, 'b) pattern -> ('a, 'c * 'b) pattern
  | Snd  : ('a, 'b) pattern * 'c -> ('a, 'b * 'c) pattern

and 'a ty =
  | TInt : int ty
  | TString : string ty
  | TFloat : float ty
  | TIdentifier : identifier ty
  | TTimestamp : timestamp ty
  | TPair : 'a ty * 'b ty -> ('a * 'b) ty

let make_predicate name visibility permission ty =
  { name; ty; visibility; permission }

module M = ArraySimpleMap.Make (struct
  type data = statement
  type key = timestamp
  let get_key (Statement (t, _, _, _)) = t
  let compare = Timestamp.compare
end)

type chunk = M.t

type statements = chunk option list

let statements : statements ref = ref []

let state who pred x =
  assert false

type 'a answers =
  | NoAnswer : 'a answers
  | Next : 'a * (unit -> 'a answers) -> 'a answers

let ask i r =
  assert false

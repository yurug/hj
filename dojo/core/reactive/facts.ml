(* -*- tuareg -*- *)

type value =
  | VInt of int
  | VIdentifier of Identifier.t
deriving (Json)

type t =
  | Property of string * value list
deriving (Json)

let valid_predicate_spec =
  Str.regexp "[a-zA-Z0-9]+"

let valid_predicate x =
  Str.string_match valid_predicate_spec x 0

exception InvalidPredicateName of string

let property x vs =
  if not (valid_predicate x) then raise (InvalidPredicateName x);
  Property (x, vs)

(* FIXME: We may need a more efficient representation here. *)
type properties = t list deriving (Json)

let empty = []

let is = List.mem

let assign s p = if not (is p s) then p :: s else s

let unassign s p = List.filter (( <> ) p) s

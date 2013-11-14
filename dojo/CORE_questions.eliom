(** -*- tuareg -*- *)

(** This module defines the language of exercise description.

    Roughly speaking, a program written in that language evaluates into
    a sequence of bindings for values of the following kinds:

    - documents that describe the statement of the exercise ;

    - evaluation specifiers that describe the protocol of evaluation,
    i.e. how the answers are submitted, how the submissions are
    evaluated and how the diagnostic is transmitted to the users.

    A basic import mechanisms enable a subsequence of bindings of
    some exercise to be used in another one.
*)

{shared{

open Lwt
open CORE_identifier

type 'a enumerate =
  | All
  | Include of 'a list
  | Exclude of 'a list
  | Union of 'a enumerate list
deriving (Json)

type component =
  | Sub     of identifier * CORE_entity.timestamp
  | Binding of CORE_identifier.label option * ty option * term
  | Import  of ty enumerate * identifier * CORE_identifier.label enumerate

and t = component list

and term =
  | Lit of literal
  | Variable of variable
  | Lam of variable * ty option * term
  | App of term * term
  | IApp of term * term list

and literal =
  | LInt    of int
  | LFloat  of float
  | LString of string

and ty =
  | TApp of type_variable * ty list

and variable = CORE_identifier.label

and type_variable = TVariable of string

deriving (Json)

}}

let timestamp_of_sub cs rkey =
  let rec aux = function
    | [] -> []
    | Sub (r, ts) :: _ when r = rkey -> [ ts ]
    | _ :: qs -> aux qs
  in
  aux cs

let context_of_checkpoint cs cp =
  (* FIXME *)
  return CORE_context.Empty

let all_checkpoints cs =
  assert false

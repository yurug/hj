(* -*- tuareg -*- *)

(** Concrete syntax trees. *)
open Error
open Position
open Identifier

type t = declaration list

and declaration =
  | ValueDecl of label * ty_scheme option * term'

and term =
  | Lit of literal
  | Template of template

  | Variable of name * ty list option
  | Lam of variable * ty option * term'
  | App of term' * term'

and name =
  | Long  of identifier
  | Short of label

and template = template_atom list

and template_atom =
  | Raw  of string located
  | Code of term'

  (** Should not occur in final CST. This constructor is used in
      intermediate trees during multi-level parsing. *)
  | RawCode of string

and term' = term located

and literal =
  | LUnit
  | LInt    of int
  | LFloat  of float
  | LString of string

and ty_scheme =
  | TScheme of type_variable list * ty

and ty =
  | TApp of type_variable * ty list

and variable = Identifier.label

and type_variable = TVariable of string deriving (Json)

type 'a with_raw = string * 'a

let with_raw s cst = (s, cst)

let raw = fst

let data = snd

let rec equivalent_terms t1 t2 =
  match t1, t2 with
    | Lit l1, Lit l2 ->
      l1 = l2
    | Variable (x1, ts1), Variable (x2, ts2) ->
      x1 = x2 && ts1 = ts2
    | Lam (x1, ty1, t1), Lam (x2, ty2, t2) ->
      x1 = x2 && ty1 = ty2 && equivalent_terms' t1 t2
    | App (a1, b1), App (a2, b2) ->
      equivalent_terms' a1 a2 && equivalent_terms' b1 b2
    | _, _ ->
      false

and equivalent_terms' t1 t2 = Position.(equivalent_terms (value t1) (value t2))

let to_ast : t -> IAST.program = assert false

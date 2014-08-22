(* -*- tuareg -*- *)

(** Abstract syntax trees. *)
open Error
open Position
open Identifier

type t = declaration list

and declaration =
  | ValueDecl of label * ty option * term'

and term =
  | Lit of literal
  | Variable of label
  | Lam of variable * ty option * term'
  | App of term' * term'

and term' = AkaCST.term' * term

and literal =
  | LUnit
  | LInt    of int
  | LFloat  of float
  | LString of string

and ty =
  | TApp of type_variable * ty list

and variable = Identifier.label

and type_variable = TVariable of string

    deriving (Json)

type 'a with_raw = string * 'a

let with_raw s cst = (s, cst)

let raw = fst

let data = snd

let rec equivalent_terms t1 t2 =
  match t1, t2 with
    | Lit l1, Lit l2 ->
      l1 = l2
    | Variable x1, Variable x2 ->
      x1 = x2
    | Lam (x1, ty1, t1), Lam (x2, ty2, t2) ->
      x1 = x2 && ty1 = ty2 && equivalent_terms' t1 t2
    | App (a1, b1), App (a2, b2) ->
      equivalent_terms' a1 a2 && equivalent_terms' b1 b2
    | _, _ ->
      false

and equivalent_terms' (t1 : term') t2 = equivalent_terms (snd t1) (snd t2)

module C = AkaCST

(** [from_cst cst] turns a concrete syntax tree into an abstract syntax
    tree. This concrete syntax tree is assumed not to contain a RawCode
    literals. *)
let from_cst =
  let rec t ds = List.map declaration ds

  and declaration = function
    | C.ValueDecl (l, a, t) ->
      ValueDecl (l, option_ty a, term' t)

  and term = function
    | C.Lit l ->
      Lit (literal l)
    | C.Template t ->
      failwith "TODO"
    | C.Variable n ->
      Variable (name n)
    | C.Lam (x, a, t) ->
      Lam (x, option_ty a, term' t)
    | C.App (a, b) ->
      App (term' a, term' b)

  and name = function
    | C.Long  i -> failwith "TODO"
    | C.Short i -> i

  and term' t = (t, term (value t))

  and literal = function
    | C.LUnit -> LUnit
    | C.LInt x -> LInt x
    | C.LFloat f -> LFloat f
    | C.LString s -> LString s

  and option_ty = function
    | None -> None
    | Some t -> Some (ty t)

  and ty = function
    | C.TApp (tv, ts) ->
      TApp (type_variable tv, List.map ty ts)

  and type_variable = function C.TVariable s -> TVariable s
  in
  t

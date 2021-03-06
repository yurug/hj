(* -*- tuareg -*- *)

{shared{

type t = string deriving (Json)

}}

let valid_atom_spec =
  Str.regexp "[a-zA-Z0-9]+"

let valid_atom x =
  Str.string_match valid_atom_spec x 0

let atom x =
  assert (valid_atom x);
  x

(* FIXME: We may need a more efficient representation here. *)
type set = t list deriving (Json)

let empty = []

let is = List.mem

let assign s p = p :: s

let unassign s p = List.filter (( <> ) p) s

let string_of_set s = "{" ^ String.concat ", " s ^ "}"

{shared{

type binop = And | Or deriving (Json)

type unop = Not deriving (Json)

type rule =
  | True
  | False
  | Is    of t
  | BinOp of binop * rule * rule
  | UnOp  of unop * rule
deriving (Json)

}}

let conj a b =
  match a, b with
    | False, _ | _, False -> False
    | True, a | a, True -> a
    | a, b -> BinOp (And, a, b)

let conjs = List.fold_left conj True

let disj a b =
  match a, b with
    | True, _ | _, True -> True
    | False, a | a, False -> a
    | a, b -> BinOp (Or, a, b)

let disjs = List.fold_left disj False

let rec evaluate s = function
  | True -> true
  | False -> false
  | Is x -> is x s
  | BinOp (And, r1, r2) -> evaluate s r1 && evaluate s r2
  | BinOp (Or, r1, r2) -> evaluate s r1 || evaluate s r2
  | UnOp (Not, r) -> not (evaluate s r)

(* FIXME: use ocamllex/menhir instead. *)

(* r := true | a | ( op r r ) *)
type token = LPAREN | RPAREN | BINOP of binop | UNOP of unop
             | IS of t | TRUE | FALSE

exception PropertyRuleSyntaxError of string

let rule_of_string s =
  let error () = raise (PropertyRuleSyntaxError s) in
  let layout = Str.regexp "[ \t\n]+" in
  let atoms = Str.split layout s in
  let tokens : token list = List.map (function
    | "(" -> LPAREN
    | ")" -> RPAREN
    | "and" -> BINOP And
    | "or" -> BINOP Or
    | "not" -> UNOP Not
    | "true" -> TRUE
    | "false" -> FALSE
    | x -> try IS (atom x) with Failure _ -> error ()
  ) atoms
  in
  let accept tok = function
    | [] -> error ()
    | tok' :: _ when tok' <> tok -> error ()
    | _ :: toks -> toks
  in
  let current : token list -> token = function
    | [] -> error ()
    | tok :: _ -> tok
  in
  let rec parse (tokens : token list) : token list * rule =
    match current tokens with
      | TRUE ->
        (accept TRUE tokens, True)
      | FALSE ->
        (accept FALSE tokens, False)
      | (IS x) as tok ->
        (accept tok tokens, Is x)
      | LPAREN as tok ->
        let tokens = accept tok tokens in
        begin
          let op = current tokens in
          match op with
            | UNOP u ->
              let tokens = accept op tokens in
              let tokens, r = parse tokens in
              let tokens = accept RPAREN tokens in
              (tokens, UnOp (u, r))
            | BINOP b ->
              let tokens = accept op tokens in
              let tokens, r1 = parse tokens in
              let tokens, r2 = parse tokens in
              let tokens = accept RPAREN tokens in
              (tokens, BinOp (b, r1, r2))
            | _ -> error ()
        end
      | _ ->
        error ()
  in
  snd (parse tokens)

let binop_to_string = function
  | And -> "and"
  | Or -> "or"

let unop_to_string = function
  | Not -> "not"

let rec rule_to_string = function
  | True ->
    "true"
  | False ->
    "false"
  | Is x ->
    x
  | BinOp (b, r1, r2) ->
    Printf.sprintf
      "(%s %s %s)"
      (binop_to_string b)
      (rule_to_string r1)
      (rule_to_string r2)
  | UnOp (b, r) ->
    Printf.sprintf
      "(%s %s)"
      (unop_to_string b)
      (rule_to_string r)

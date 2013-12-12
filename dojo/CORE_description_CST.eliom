(* -*- tuareg -*- *)

(** Concrete syntax trees. *)
{shared{

open CORE_errors

type 'a located = {
  node     : 'a;
  start    : position;
  stop     : position;
}
deriving (Json)

let start_of x = x.start

let stop_of x = x.stop

let lexing_locate pstart pstop node = {
  node;
  start = from_lexing_position pstart;
  stop = from_lexing_position pstop;
}

let locate start stop node = {
  start; stop; node
}

let locate_as x node = {
  start = x.start;
  stop = x.stop;
  node
}

type 'a enumerate =
  | All
  | Insert of 'a list
  | Remove of 'a list
  | Union of 'a enumerate list
deriving (Json)

type exercise = {
  title : string located;
  questions : t;
}

and component =
  | Include     of identifier * position * position
  | Sub         of identifier * exercise located
  | Binding     of binding
  | Import      of ty enumerate * identifier * CORE_identifier.label enumerate

and binding = CORE_identifier.label option * ty option * term located

and t = component list

and term =
  | Lit of literal
  | Template of template
  | Variable of variable
  | Lam of variable * ty option * term located
  | App of term located * term located
  | IApp of term located * term located list
  (* Syntactic sugars. *)
  | Seq of term located list

and template = template_atom list

and template_atom =
  | Raw  of string
  | Code of term located
  | RawCode of string (** Should not occur in final AST. *)

and term' = term located

and literal =
  | LUnit
  | LInt    of int
  | LFloat  of float
  | LString of string

and ty =
  | TApp of type_variable * ty list

and variable = CORE_identifier.label

and type_variable = TVariable of string

and identifier = string located

deriving (Json)

type 'a with_raw = string * 'a deriving (Json)

let with_raw s cst = (s, cst)

let raw = fst

let data = snd

let rec equivalent_exercises e1 e2 =
  e1.title.node = e2.title.node &&
  equivalent_questions e1.questions e2.questions

and equivalent_questions qs1 qs2 =
  List.length qs1 = List.length qs2 &&
  List.for_all2 equivalent_components qs1 qs2

and equivalent_components q1 q2 =
  match q1, q2 with
    | Include (s1, _, _), Include (s2, _, _) ->
      s1.node = s2.node
    | Sub (s1, e1), Sub (s2, e2) ->
      s1.node = s2.node && equivalent_exercises e1.node e2.node
    | Import (tys1, xs1, e1), Import (tys2, xs2, e2) ->
      tys1 = tys2 && xs1 = xs2 && e1 = e2
    | Binding (x1, ty1, t1), Binding (x2, ty2, t2) ->
      x1 = x2 && ty1 = ty2 && equivalent_terms t1.node t2.node
    | _, _ -> false

and equivalent_terms t1 t2 =
  match t1, t2 with
    | Lit l1, Lit l2 ->
      l1 = l2
    | Variable x1, Variable x2 ->
      x1 = x2
    | Lam (x1, ty1, t1), Lam (x2, ty2, t2) ->
      x1 = x2 && ty1 = ty2 && equivalent_terms' t1 t2
    | App (a1, b1), App (a2, b2) ->
      equivalent_terms' a1 a2 && equivalent_terms' b1 b2
    | IApp (t1, ts1), IApp (t2, ts2) ->
      equivalent_terms t1.node t2.node
      && List.for_all2 equivalent_terms' ts1 ts2
    | _, _ ->
      false

and equivalent_terms' t1 t2 = equivalent_terms t1.node t2.node

let dummy_position = { line = -1; character = -1 }

let dummy_loc x = { node = x; start = dummy_position; stop = dummy_position }

let blank = {
  title = dummy_loc I18N.String.no_title;
  questions = []
}

(** precondition: Assume that [s] contains a least [l] lines. *)
let offset_of { line = l; character = c } s =
  let rec goto_line b i =
    if i = 0 then b
    else if s.[b] = '\n' then goto_line (b + 1) (i - 1)
    else goto_line (b + 1) i
  in
  goto_line 0 (l - 1) + (c - 1)

let slice start stop s =
  let ostart = offset_of start s in
  let ostop = offset_of stop s in
  String.sub s ostart (ostop - ostart + 1)

}}

let with_sub_raw raw n =
  Ocsigen_messages.errlog (Printf.sprintf "Slice %d-%d %d-%d\n%s\n"
                            n.start.line n.start.character
                            n.stop.line n.stop.character
                            raw);
  (slice n.start n.stop raw, n.node)

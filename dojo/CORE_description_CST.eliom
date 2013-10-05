(* -*- tuareg -*- *)

(** Concrete syntax trees. *)
{shared{

type position = { line : int; character : int } deriving (Json)

let from_lexing_position p =
  { line      = p.Lexing.pos_lnum;
    character = p.Lexing.pos_cnum - p.Lexing.pos_bol;
  }

type 'a located = {
  node     : 'a;
  start    : position;
  stop     : position;
}
deriving (Json)

let locate pstart pstop node = {
  node;
  start = from_lexing_position pstart;
  stop = from_lexing_position pstop;
}

type exercise = {
  title : string located;
  questions : questions;
}

and questions =
  | Compose     of composer * questions list
  | Statement   of string located * questions
  | ContextRule of context_rule * questions
  | Checkpoint  of identifier * questions
  | Include     of identifier * position * position
  | Sub         of identifier * exercise located option

and composer = Seq | Par

and identifier = string located

and context_rule =
  | Answer of string

deriving (Json)

exception ParseError of Lexing.position * Lexing.position * string

type 'a with_raw = string * 'a deriving (Json)

let with_raw s cst = (s, cst)

let raw = fst

let data = snd

let rec equivalent_exercises e1 e2 =
  e1.title.node = e2.title.node &&
  equivalent_questions e1.questions e2.questions

and equivalent_questions q1 q2 =
  match q1, q2 with
    | Compose (c1, qs1), Compose (c2, qs2) when c1 = c2 ->
      (try List.for_all2 equivalent_questions qs1 qs2 with _ -> false)
    | Statement (s1, q1), Statement (s2, q2) ->
      s1.node = s2.node && equivalent_questions q1 q2
    | ContextRule (r1, q1), ContextRule (r2, q2) ->
      equivalent_context_rule r1 r2 && equivalent_questions q1 q2
    | Checkpoint (c1, q1), Checkpoint (c2, q2) ->
      c1.node = c2.node && equivalent_questions q1 q2
    | Include (s1, _, _), Include (s2, _, _) ->
      s1.node = s2.node
    | Sub (s1, None), Sub (s2, None) ->
      s1.node = s2.node
    | Sub (s1, Some e1), Sub (s2, Some e2) ->
      s1.node = s2.node && equivalent_exercises e1.node e2.node
    | _, _ -> false

and equivalent_context_rule r1 r2 =
  match r1, r2 with
    | Answer fname1, Answer fname2 ->
      fname1 = fname2
    | _, _ -> false

let dummy_position = { line = -1; character = -1 }

let dummy_loc x = { node = x; start = dummy_position; stop = dummy_position }

let blank = {
  title = dummy_loc I18N.String.no_title;
  questions = Compose (Seq, [])
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

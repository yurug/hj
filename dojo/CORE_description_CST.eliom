(* -*- tuareg -*- *)

(** Concrete syntax trees. *)
{shared{

type position = { line : int; character : int } deriving (Json)

let from_lexing_position p =
  { line = p.Lexing.pos_lnum; character = p.Lexing.pos_cnum }

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

type questions =
  | Compose of composer * questions list
  | Single  of question

and composer = Seq | Par

and question =
  | Question of identifier * question_definition option

and question_definition = {
  statement : string located;
}

and identifier = string located

deriving (Json)

exception ParseError of Lexing.position * Lexing.position * string

type 'a with_raw = string * 'a deriving (Json)

let with_raw s cst = (s, cst)

let raw = fst

let data = snd

}}

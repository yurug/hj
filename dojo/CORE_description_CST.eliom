(** Concrete syntax trees. *)
{shared{

type 'a located = {
  node     : 'a;
  start    : Lexing.position;
  stop     : Lexing.position;
}

let locate start stop node = { node; start; stop }

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

}}
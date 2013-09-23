(** Lexers for description languages. *)

{
  open CORE_description_parser
  open Lexing

  type error = [
    `UnexpectedEOFinRaw
  ]

  exception LexicalError of error


 (** This function increments the line number in the buffer [lexbuf]
     and calls [f] on it. *)
  let next_line_and f lexbuf  =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with
        pos_lnum = pos.pos_lnum + 1;
        pos_bol  = pos.pos_cnum;
      };
    f lexbuf

}

(** Layout. *)

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

(** Identifier. *)

let label = [ 'a' - 'z' 'A' - 'Z' '0' - '9' ]+

let identifier = label ('/' label)*

rule main = parse
(** Layout. *)
| newline                               { next_line_and main lexbuf }
| blank+                                { main lexbuf }
| eof                                   { EOF }

(** Operators. *)
| "or else"                             { ORELSE }
| "then"                                { THEN   }

(** Punctuations. *)
| "("                                   { LPAREN }
| ")"                                   { RPAREN }

(** Literal. *)
| "{"                                   { raw (Buffer.create 13) 0 lexbuf }
| identifier as id                      { ID id }

| _ {
  raise (CORE_description_CST.ParseError (
    lexbuf.Lexing.lex_start_p,
    lexbuf.Lexing.lex_curr_p,
    "Lexing"))
}

and raw chunk level = parse
  | "}" {
    if level = 0 then
      RAW (Buffer.contents chunk)
    else (
      Buffer.add_char chunk '}';
      raw chunk (level - 1) lexbuf
    )
  }
  | "{" {
    Buffer.add_char chunk '{';
    raw chunk (level + 1) lexbuf
  }
  | eof {
    raise (LexicalError `UnexpectedEOFinRaw)
  }
  | _ as c {
    Buffer.add_char chunk c;
    raw chunk level lexbuf
  }

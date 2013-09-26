(** Lexers for description languages. *)

{
  open CORE_description_CST
  open CORE_description_parser
  open Lexing

  let error lexbuf e =
    raise (ParseError (lexbuf.lex_start_p, lexbuf.lex_curr_p, e))

 (** This function increments the line number in the buffer [lexbuf]
     and calls [f] on it. *)
  let next_line_and f lexbuf  =
    Lexing.new_line lexbuf;
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
| "{"                                   {
  let p = lexbuf.Lexing.lex_curr_p in
  let token = raw (Buffer.create 13) 0 lexbuf in
  lexbuf.lex_start_p <- p;
  token
}
| identifier as id                      { ID id }

| _ {
  error lexbuf I18N.String.lexing_unexpected_character
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
    error lexbuf I18N.String.lexing_eof_in_raw
  }
  | newline as c {
    Buffer.add_string chunk c;
    next_line_and (raw chunk level) lexbuf
  }
  | _ as c {
    Buffer.add_char chunk c;
    raw chunk level lexbuf
  }

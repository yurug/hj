(** Lexers for description languages. *)

{
  open CORE_errors
  open CORE_description_CST
  open CORE_description_parser
  open Lexing

  let error lexbuf e =
    raise (ParseError (from_lexing_position lexbuf.lex_start_p,
                       from_lexing_position lexbuf.lex_curr_p,
                       e))

 (** This function increments the line number in the buffer [lexbuf]
     and calls [f] on it. *)
  let next_line_and f lexbuf  =
    Lexing.new_line lexbuf;
    f lexbuf

  let raw_string lexbuf chunk =
    Lexing.(CORE_description_CST.lexing_locate
              lexbuf.lex_start_p lexbuf.lex_curr_p (
                Buffer.contents chunk
              )
    )

}

(** Layout. *)

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

(** Identifier. *)

let label = [ 'a' - 'z' 'A' - 'Z' '0' - '9' '-' '_' ]+

let digit = [ '0' - '9' ]

let identifier = label ('/' label)*

let number = digit+

rule main = parse
(** Layout. *)
| newline                               { next_line_and main lexbuf }
| blank+                                { main lexbuf }
| eof                                   { EOF }

| "{*"                                  { comment 1 lexbuf }

(** Keywords. *)
| "do"                                  { DO }
| "from"                                { FROM }

(** Punctuations. *)
| "("                                   { LPAREN }
| ")"                                   { RPAREN }
| "{"                                   { LBRACE }
| "}"                                   { RBRACE }
| "?"                                   { QMARK }
| ","                                   { COMMA }
| ":"                                   { COLON }
| ";"                                   { SEMICOLON }
| "."                                   { DOT }

(** Operators. *)
| "-"                                   { MINUS }
| "+"                                   { PLUS }
| "*"                                   { STAR }
| "="                                   { EQUAL }
| "->"                                  { RARROW }

(** Literal. *)
| number as x {
  INT (int_of_string x)
}

| "[[" | "[[\n" {
 let p = lexbuf.Lexing.lex_curr_p in
  let token = flatraw (Buffer.create 13) 0 lexbuf in
  lexbuf.lex_start_p <- p;
  token
}

| "\"" | "\"\n" {
 let p = lexbuf.Lexing.lex_curr_p in
  let token = flatstring (Buffer.create 13) 0 lexbuf in
  lexbuf.lex_start_p <- p;
  token
}


| "[" | "[\n"                           {
  let p = lexbuf.Lexing.lex_curr_p in
  let token = raw (Buffer.create 13) [] 0 lexbuf in
  lexbuf.lex_start_p <- p;
  token
}

| "<<" {
  let p = lexbuf.Lexing.lex_curr_p in
  let token = textblock (Buffer.create 13) [] [] 0 lexbuf in
  lexbuf.lex_start_p <- p;
  token
}

| '#' (identifier as id)                  { ID id }

| label as id                           { NAME id }

| _ {
  error lexbuf I18N.String.lexing_unexpected_character
}

and raw chunk template level = parse
  | "]" | "\n]" {
    if level = 0 then
      RAW (List.rev (Raw (raw_string lexbuf chunk) :: template))
    else (
      let template =
        if level = 1 then
          let atom = RawCode (Buffer.contents chunk) in (
            Buffer.clear chunk;
            atom :: template
          ) else (
            Buffer.add_char chunk ']';
            template
          )
      in
      raw chunk template (level - 1) lexbuf
    )
  }
  | "[" | "[\n" {
    let template =
      if level = 0 then
        let atom = Raw (raw_string lexbuf chunk) in (
          Buffer.clear chunk;
          atom :: template
        )
      else (
        Buffer.add_char chunk '[';
        template
      )
    in
    raw chunk template (level + 1) lexbuf
  }
  | "\\[" {
    Buffer.add_char chunk '[';
    raw chunk template level lexbuf
  }
  | "\\]" {
    Buffer.add_char chunk ']';
    raw chunk template level lexbuf
  }
  | eof {
    error lexbuf I18N.String.lexing_eof_in_raw
  }
  | newline as c {
    Buffer.add_string chunk c;
    next_line_and (raw chunk template level) lexbuf
  }
  | _ as c {
    Buffer.add_char chunk c;
    raw chunk template level lexbuf
  }


and textblock chunk template templates level = parse
  | ">>" {
    let template = List.rev (Raw (raw_string lexbuf chunk) :: template) in
    let templates = template :: templates in
    TEXTBLOCK (List.rev templates)
  }
  | "]" {
    let template =
      if level = 1 then
        let atom = RawCode (Buffer.contents chunk) in (
          Buffer.clear chunk;
          atom :: template
        ) else (
          Buffer.add_char chunk ']';
          template
        )
    in
    textblock chunk template templates (level - 1) lexbuf
  }
  | "[" | "[\n" {
    let template =
      if level = 0 then
        let atom = Raw (raw_string lexbuf chunk) in (
          Buffer.clear chunk;
          atom :: template
        )
      else (
        Buffer.add_char chunk '[';
        template
      )
    in
    textblock chunk template templates (level + 1) lexbuf
  }
  | "\\[" {
    Buffer.add_char chunk '[';
    textblock chunk template templates level lexbuf
  }
  | "\\]" {
    Buffer.add_char chunk ']';
    textblock chunk template templates level lexbuf
  }
  | newline blank* newline {
    let template =
      let atom = Raw (raw_string lexbuf chunk) in
      Buffer.clear chunk;
      atom :: template
    in
    let templates = template :: templates in
    let template = [] in
    next_line_and (textblock chunk template templates level) lexbuf
  }
  | eof {
    error lexbuf I18N.String.lexing_eof_in_raw
  }
  | newline as c {
    Buffer.add_string chunk c;
    next_line_and (textblock chunk template templates level) lexbuf
  }
  | _ as c {
    Buffer.add_char chunk c;
    textblock chunk template templates level lexbuf
  }

and flatraw chunk level = parse
  | ("]]" | "\n]]") as s {
    if level = 0 then
      RAW [Raw (raw_string lexbuf chunk)]
    else (
      Buffer.add_string chunk s;
      flatraw chunk (level - 1) lexbuf
    )
  }
  | "[[" {
    Buffer.add_string chunk "[[";
    flatraw chunk (level + 1) lexbuf
  }
  | eof {
    error lexbuf I18N.String.lexing_eof_in_raw
  }
  | newline as c {
    Buffer.add_string chunk c;
    next_line_and (flatraw chunk level) lexbuf
  }
  | _ as c {
    Buffer.add_char chunk c;
    flatraw chunk level lexbuf
  }

and flatstring chunk level = parse
  | ("\"" | "\n\"") as s {
    RAW [Raw (raw_string lexbuf chunk)]
  }
  | "\\\"" {
    Buffer.add_string chunk "\"";
    flatstring chunk level lexbuf
  }
  | eof {
    error lexbuf I18N.String.lexing_eof_in_raw
  }
  | newline as c {
    Buffer.add_string chunk c;
    next_line_and (flatstring chunk level) lexbuf
  }
  | _ as c {
    Buffer.add_char chunk c;
    flatstring chunk level lexbuf
  }

and comment level = parse
  | "*}" {
    if level = 1 then
      main lexbuf
    else
      comment (pred level) lexbuf
  }
  | "{*" {
    comment (succ level) lexbuf
  }
  | eof {
    error lexbuf I18N.String.lexing_eof_in_raw
  }
  | newline as c {
    next_line_and (comment level) lexbuf
  }
  | _ {
    comment level lexbuf
  }

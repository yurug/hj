(** Lexers for description languages. *)

{
  open Error
  open Position
  open AkaCST
  open AkaParser
  open Lexing
  open Identifier

  exception NonTerminatedComment

  exception UnexpectedCharacter of char

  let error lexbuf =
    raise (AkaError.Parse (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p))

 (** This function increments the line number in the buffer [lexbuf]
     and calls [f] on it. *)
  let next_line_and f lexbuf  =
    Lexing.new_line lexbuf;
    f lexbuf

  let count_next_line_and s f lexbuf =
    if String.contains s '\n' then
      next_line_and f lexbuf
    else
      f lexbuf

  let fresh lexbuf =
    Position.with_cpos lexbuf ""

  let next lexbuf = function
    | RawCode s :: t ->
      Raw (fresh lexbuf) :: RawCode s :: t
    | Raw s :: t ->
      RawCode (fresh lexbuf) :: Raw s :: t
    | [] ->
      Raw (fresh lexbuf) :: []
    | _ -> assert false

  let concat lexbuf s s' =
    let v = (value s ^ s') in
    let endpos = lex_join lexbuf.lex_curr_p lexbuf.lex_curr_p in
    let pos = join (position s) endpos in
    with_pos pos v

  let push_string lexbuf s' = function
    | RawCode s :: t -> RawCode (concat lexbuf s s') :: t
    | Raw s :: t -> Raw (concat lexbuf s s') :: t
    | _ -> assert false

  let push lexbuf c = push_string lexbuf (String.make 1 c)

}

(** Layout. *)

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

(** Identifier. *)

let label = [ 'a' - 'z' 'A' - 'Z' '0' - '9' '-' '_' ]+

let ulabel = ['A' - 'Z' ] [ 'a' - 'z' 'A' - 'Z' '0' - '9' '-' '_' ]*

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
| "def"                                 { DEF }
| "data"                                { DATATYPE }
| "and"                                 { AND }
| "int"                                 { TINT }
| "char"                                { TCHAR }
| "unit"                                { TUNIT }
| "as"                                  { AS }
| "import"                              { IMPORT }
| "do"                                  { DO }

(** Punctuations. *)
| "?"                                   { QMARK }
| "."                                   { DOT }
| "#"                                   { SHARP }
| "_"                                   { UNDERSCORE }
| "="                                   { EQUAL }
| ","                                   { COMMA }
| ":"                                   { COLON }
| ";"                                   { SEMICOLON }
| "|"                                   { PIPE }
| "->"                                  { RARROW }
| "=>"                                  { DRARROW }
| ">>"                                  { RRPAREN }
| "<<"                                  { LLPAREN }
| "{"                                   { LBRACE }
| "}"                                   { RBRACE }
| "("                                   { LPAREN }
| ")"                                   { RPAREN }
| ("["+) as opening {
  let closing = String.make (String.length opening) ']' in
  template opening closing 1 (next lexbuf []) lexbuf
}

(** Operators. *)

(** Literal. *)
| number as x {
  INT (int_of_string x)
}

| ulabel as uid                         { UNAME uid }

| label as id                           { NAME id }

| (label ("/" label)+) as s             { IDENTIFIER (identifier_of_string s) }

| ("'" label) as id                     { TVNAME id }

| _ {
  error lexbuf
}

and template osym csym level accu = parse
| ("]"+ as closing) {
  if closing = csym then(
    if level = 1 then
      TEMPLATE (List.rev accu)
    else
      template osym csym (level - 1) (next lexbuf accu) lexbuf
  ) else
    template osym csym level (push_string lexbuf closing accu) lexbuf
}
| ("["+ as opening) {
  if opening = osym then
    template osym csym (level + 1) (next lexbuf accu) lexbuf
  else
    template osym csym level (push_string lexbuf opening accu) lexbuf
}
| eof {
  error lexbuf
}
| newline {
  next_line_and (template osym csym level (push lexbuf '\n' accu)) lexbuf
}
| _ as c {
  template osym csym level (push lexbuf c accu) lexbuf
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
    error lexbuf
  }
  | newline {
    next_line_and (comment level) lexbuf
  }
  | _ {
    comment level lexbuf
  }

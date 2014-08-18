(** Lexers for description languages. *)

{
  open Error
  open Position
  open AkaCST
  open AkaParser
  open Lexing

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
| "def"                                 { DEF }

(** Punctuations. *)
| "="                                   { EQUAL }

(** Operators. *)

(** Literal. *)
| number as x {
  INT (int_of_string x)
}

| label as id                           { NAME id }

| _ {
  error lexbuf
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

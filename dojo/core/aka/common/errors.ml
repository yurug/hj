(** Error messages. *)

open Position
open Lexing

let string_of_positions ps =
  String.concat "\n" (List.map string_of_lex_pos ps)

let string_of_positions = function
  | [] -> ""
  | [start] -> string_of_pos (lex_join start start)
  | [start; stop] -> string_of_pos (lex_join start stop)
  | ps -> string_of_positions ps

exception AkaError of string

let fatal pos msg =
  Error.fatal (
    AkaError (Printf.sprintf "%s:\n%s\n" (string_of_positions pos) msg)
  )

let fatal' pos msg =
  Error.fatal (
    AkaError (Printf.sprintf "%s:\n%s\n" (string_of_pos pos) msg)
  )

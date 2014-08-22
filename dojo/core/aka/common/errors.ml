(** Error messages. *)

open Positions
open Lexing

let string_of_positions ps =
  String.concat "\n" (List.map string_of_lex_pos ps)

let string_of_positions = function
  | [] -> ""
  | [start] -> string_of_pos (lex_join start start)
  | [start; stop] -> string_of_pos (lex_join start stop)
  | ps -> string_of_positions ps

let fatal pos msg =
  Printf.eprintf "%s:\n%s\n" (string_of_positions pos) msg;
  exit 1

let fatal' pos msg =
  Printf.eprintf "%s:\n%s\n" (string_of_pos pos) msg;
  exit 1

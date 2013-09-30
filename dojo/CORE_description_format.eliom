(** -*- tuareg -*- *)

{shared{

open Lwt
open CORE_description_parser
open CORE_description_lexer
open CORE_description_CST

let process ~lexer_init ~lexer_fun ~parser_fun ~input =
  parser_fun lexer_fun (lexer_init input)

let process ~lexer_init ~lexer_fun ~parser_fun ~input  =
  try
    `OK (process ~lexer_init ~lexer_fun ~parser_fun ~input)
  with
    | Sys_error msg -> `KO (`SystemError msg)
    | ParseError (start, stop, msg) -> `KO (`SyntaxError (start, stop, msg))

let process_string parser_fun s =
  let ret =
    process ~lexer_init:Lexing.from_string ~lexer_fun:main ~parser_fun ~input:s
  in
  match ret with
    | `OK cst -> `OK (with_raw s cst)
    | `KO e -> `KO e

let exercise_of_string = process_string description

}}

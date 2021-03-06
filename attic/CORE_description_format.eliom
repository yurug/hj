(** -*- tuareg -*- *)

{client{
  let trace m = Firebug.console##log (Js.string m)
}}

{server{
  let trace m = ()
}}


{shared{

open Lwt
open CORE_errors
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
    | e -> `KO (`SystemError (Printexc.to_string e))

let process_string parser_fun s =
  let ret =
    process ~lexer_init:Lexing.from_string ~lexer_fun:main ~parser_fun ~input:s
  in
  match ret with
    | `OK cst -> `OK (with_raw s cst)
    | `KO e -> `KO e

exception Error of CORE_errors.all

let term_of_string s =
  process_string topterm s

let exercise_of_string s =
  let rec aux s =
    match process_string description s with
      | `OK cst -> aux' cst
      | `KO e -> `KO e
  and aux' (raw, cst) =
    let rec exercise e = term' e
    and term' t = locate_as t (term t.node)
    and term = function
      | Template t -> Template (template t)
      | Lam (x, ty, t) -> Lam (x, ty, term' t)
      | App (a, b) -> App (term' a, term' b)
      | Module m -> Module (module_term m)
      | t -> t
    and module_term m = List.map module_component m
    and module_component (l, ty, t) = (l, ty, term' t)
    and template t = List.map template_atom t
    and template_atom = function
      | RawCode p -> begin match term_of_string p with
          | `OK t -> Code (term' (snd t))
          | `KO e -> raise (Error e)
      end
      | a -> a
    in
    try
      `OK (raw, exercise cst)
    with Error e -> `KO e
  in
  aux s


}}

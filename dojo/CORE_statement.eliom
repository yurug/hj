(** -*- tuareg -*- *)

{client{

open Lwt
open CORE_errors
open CORE_statement_parser
open CORE_statement_lexer
open CORE_statement_AST

(* FIXME: Factorize with CORE_description_format. *)
let process ~lexer_init ~lexer_fun ~parser_fun ~input =
  parser_fun lexer_fun (lexer_init input)

let process ~lexer_init ~lexer_fun ~parser_fun ~input  =
  try
    `OK (process ~lexer_init ~lexer_fun ~parser_fun ~input)
  with
    | Sys_error msg -> `KO (`SystemError msg)
    | ParseError (start, stop, msg) -> `KO (`SyntaxError (start, stop, msg))

let process_string parser_fun s =
  process ~lexer_init:Lexing.from_string ~lexer_fun:main ~parser_fun ~input:s

let structure_of_string =
  process_string toplevel_structure

let statement_of_string s =
  let flow = Regexp.(split (regexp "\n[ ]*\n+") s) in
  let rec aux = function
    | [] -> `OK []
    | s :: ss ->
      Firebug.console##log_2 (Js.string "Parse", Js.string s);
      match structure_of_string s, aux ss with
        | `OK (Structure (l, b, s1)), `OK s2 ->
           `OK (Structure (l, b, s1) :: s2)
        | `KO e, `KO _ -> `KO e
        | `KO e, _ -> `KO e
        | _, _ -> assert false
  in
  aux flow

open Eliom_content
open Html5.D

let html_of_statement d =
  let rec document (Structure (l, b, s)) : [< Html5_types.div_content ] elt =
    div (structure_level (body b) (List.map document s) l)
  and structure_level w c = function
    | Paragraph -> p w :: c
    | Section -> h2 w :: c
    | Subsection -> h3 w :: c
    | List -> [ul (List.map (fun x -> li [x]) c)]
    | Enumeration -> [ol (List.map (fun x -> li [x]) c)]
  and body b = List.flatten (List.map atom b)
  and atom = function
    | Raw s -> [pcdata s]
    | Image s -> [pcdata ("IMAGE:" ^ s)]
    | _ -> [pcdata "FIXME"]
  in
  div (List.map document d)

let string_of_statement d =
  let rec document indent (Structure (l, b, s)) =
    Printf.sprintf "%sstructure %s [%s]\n%s"
      (String.make indent ' ')
      (structure_level l) (body b)
      (String.concat "\n" (List.map (document (indent + 2)) s))
  and structure_level = function
    | Paragraph -> "p"
    | Section -> "sec"
    | Subsection -> "subsec"
  and body b = String.concat " " (List.map atom b)
  and atom = function
    | Raw s -> s
    | Image s-> "IMAGE(" ^ s ^ ")"
    | _ -> "FIXME"
  in
  String.concat "\n" (List.map (document 0) d)

let html_of_string s =
  match statement_of_string s with
    | `OK s ->
(*      Firebug.console##log (string_of_statement s);*)
      html_of_statement s
    | `KO s -> div [pcdata "ERROR"] (* FIXME *)

}}

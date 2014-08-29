open Lwt
open Position
open Identifier

type t =
    AkaCST.t
deriving (Json)

let parse source_code = AkaCST.(
  let step input =
    SyntacticAnalysis.process
      ~lexer_init:Lexing.from_string
      ~lexer_fun:AkaLexer.main
      ~parser_fun:AkaParser.top_term
      ~input
  in
  let toplevel input =
    SyntacticAnalysis.process
      ~lexer_init:Lexing.from_string
      ~lexer_fun:AkaLexer.main
      ~parser_fun:AkaParser.program
      ~input
  in
  let rec aux ast =
    substitute_template_in_term aux_term ast
  and aux_term = function
    | RawCode s ->
      let spos = Position.(start_of_position (position s)) in
      let ast = aux (step (Position.value s)) in
      let change_pos t =
        Position.(with_pos (shift spos (position t)) (value t))
      in
      (* FIXME: Rewrite relative positions! *)
      Code (substitute_term_in_term change_pos ast)
    | t -> t
  in
  substitute_template_in_program aux_term (toplevel source_code)
)

let typecheck module_name cst =
  lwt ast = AkaCST.to_ast module_name cst in
(*  Printf.printf "Implicit:\n %s\n%!" (ASTio.(to_string IAST.pprint_program ast));*)
  let typed_ast = InferTypes.program ast in
(*  Printf.printf "Explicit:\n %s\n%!" (ASTio.(to_string XAST.pprint_program typed_ast));*)
  return typed_ast

let compile module_name source_code =
  ExtPervasives.how_long "compile" (fun () ->
    let cst = parse source_code in
    lwt typed_ast = typecheck module_name cst in
    return (cst, typed_ast)
  )

let execute module_name cst =
  lwt typed_ast = typecheck module_name cst in
  AkaInterpreter.program typed_ast

let extract_questions env uid = XAST.(
  let user =
    EPrimitive (dummy, PStringConstant (string_of_identifier uid))
  in
  let questions_fun =
    EVar (dummy, Name.Name "__questions__", [])
  in
  let target =
    EDCon (dummy, Name.DName "ForUser", [], [ user ])
  in
  let extractor =
    EApp (dummy, questions_fun, target)
  in
  AkaInterpreter.expression env extractor
)

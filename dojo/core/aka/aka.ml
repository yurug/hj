open Lwt

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
    substitute_term aux_term ast
  and aux_term = function
    | RawCode s ->
      let _pos = Position.position s in
      let ast = aux (step (Position.value s)) in
      (* FIXME: Rewrite relative positions! *)
      Code ast
    | t -> t
  in
  substitute_template aux_term (toplevel source_code)
)

let typecheck cst =
  lwt ast = AkaCST.to_ast cst in
  Printf.printf "Implicit:\n %s\n%!" (ASTio.(to_string IAST.pprint_program ast));
  let typed_ast = InferTypes.program ast in
  Printf.printf "Explicit:\n %s\n%!" (ASTio.(to_string XAST.pprint_program typed_ast));
  return typed_ast

let compile source_code =
  ExtPervasives.how_long "compile" (fun () ->
    let cst = parse source_code in
    lwt typed_ast = typecheck cst in
    return (cst, typed_ast)
  )

let execute cst =
  lwt typed_ast = typecheck cst in
  AkaInterpreter.program typed_ast

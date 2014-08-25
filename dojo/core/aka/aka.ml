type t =
    unit
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

let compile source_code =
  let cst = parse source_code in
  let ast = AkaCST.to_ast cst in
  Printf.printf "Implicit:\n %s\n%!" (ASTio.(to_string IAST.pprint_program ast));
  let _typed_ast = InferTypes.program ast in
  Printf.printf "Explicit:\n %s\n%!" (ASTio.(to_string XAST.pprint_program _typed_ast));
  ()

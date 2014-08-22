type t =
    unit
deriving (Json)

let compile source_code =
  let cst =
    SyntacticAnalysis.process
      ~lexer_init:Lexing.from_string
      ~lexer_fun:AkaLexer.main
      ~parser_fun:AkaParser.program
      ~input:source_code
  in
  let ast = AkaCST.to_ast cst in
  let _typed_ast = InferTypes.program ast in
  ()

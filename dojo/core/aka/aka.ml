type t =
    AkaTypeChecker.TypeEnv.t * AkaAST.t
deriving (Json)

let compile source_code =
  let cst =
    SyntacticAnalysis.process
      ~lexer_init:Lexing.from_string
      ~lexer_fun:AkaLexer.main
      ~parser_fun:AkaParser.program
      ~input:source_code
  in
  let ast =
    AkaAST.from_cst cst
  in
  let typed_ast =
    AkaTypeChecker.elaborate ast
  in
  typed_ast

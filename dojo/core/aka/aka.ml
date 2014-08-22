let compile source_code =
  SyntacticAnalysis.process
    ~lexer_init:Lexing.from_string
    ~lexer_fun:AkaLexer.main
    ~parser_fun:AkaParser.program
    ~input:source_code

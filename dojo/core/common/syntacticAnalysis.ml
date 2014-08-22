exception ParsingError

type 'token with_pos = 'token * Lexing.position * Lexing.position

let parsing_step = "during parsing"

let process ~lexer_init ~lexer_fun ~parser_fun ~input =
  parser_fun lexer_fun (lexer_init input)

exception SyntaxError of string * string

let process ~lexer_init ~lexer_fun ~parser_fun ~input  = try
  process ~lexer_init ~lexer_fun ~parser_fun ~input
with Sys_error msg ->
  Error.fatal (SyntaxError (parsing_step, msg))

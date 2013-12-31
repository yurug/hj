(** Parsers for description languages. *)

%{
  open CORE_description_CST
  open CORE_errors

  let rec app f = function
    | [] ->
      f
    | x :: xs ->
      app (locate (start_of f) (stop_of x) (App (f, x))) xs

%}

%start<CORE_description_CST.exercise> description
%start<CORE_description_CST.term CORE_description_CST.located> topterm

(** Literals. *)
%token<string> ID NAME
%token<CORE_description_CST.template_atom list> RAW
%token<int> INT
%token<float> FLOAT

(** Punctation. *)
%token EOF LPAREN RPAREN LBRACE RBRACE QMARK COMMA COLON SEMICOLON

(** Operators *)
%token MINUS PLUS STAR EQUAL RARROW

(** Keywords *)
%token FROM

(** Priorities *)
%right COMMA
%right RARROW
%left pvar
%nonassoc LPAREN

%%

topterm: t=located(term) EOF {
  t
}

description: e=exercise EOF {
  e
}

exercise: t=located(term) {
  t
}
| error {
  raise (CORE_errors.ParseError (
    from_lexing_position $startpos,
    from_lexing_position $endpos,
    I18N.String.parse_error
  ))
}

term0: n=label %prec pvar
{
  Variable (PSub (PThis, n))
}
| r=RAW
{
  Template r
}
| x=INT
{
  Lit (LInt x)
}
| f=FLOAT
{
  Lit (LFloat f)
}
| LPAREN RPAREN
{
  Lit LUnit
}
| LBRACE bs=separated_list(SEMICOLON, binding) RBRACE
{
  Module bs
}
| LPAREN t=structured_term RPAREN
{
  t
}

structured_term: a=located(term) b=located(term0)
{
  App (a, b)
}

term: t=structured_term
{
  t
}
| t=term0
{
  t
}

binding: l=label EQUAL t=located(term0)
{
  (Some l, None, t)
}
| t=located(term0)
{
  (None, None, t)
}

label: n=NAME
{
  CORE_identifier.label n
}

ty: n=NAME
{
  TApp (TVariable n, [])
}
| t1=ty RARROW t2=ty
{
  TApp (TVariable "->", [t1; t2])
}
| LPAREN t=ty RPAREN
{
  t
}

enumeration(X): PLUS s=sequence(X)
{
  Insert s
}
| MINUS s=sequence(X)
{
  Remove s
}
| e1=enumeration(X) COMMA e2=enumeration(X)
{
  match e2 with
    | Union es -> Union (e1 :: es)
    | e2 -> Union [e1; e2]
}
| LPAREN e=enumeration(X) RPAREN
{
  e
}
| STAR
{
  All
}

type_ascription: COLON ty=ty
{
  ty
}

sequence(X): LBRACE xs=separated_nonempty_list(COMMA, X) RBRACE
{
  xs
}

%inline identifier: id=located(ID) {
  id
}

%inline located(X): x=X {
  lexing_locate $startpos $endpos x
}

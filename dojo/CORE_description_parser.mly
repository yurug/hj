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

(** Literals. *)
%token<string> ID NAME RAW
%token<int> INT
%token<float> FLOAT

(** Punctation. *)
%token EOF LPAREN RPAREN LBRACE RBRACE QMARK COMMA COLON SEMICOLON

(** Operators *)
%token MINUS PLUS STAR EQUAL RARROW

(** Keywords *)
%token EXERCISE FROM

(** Priorities *)
%right COMMA
%right RARROW
%left pvar
%nonassoc LPAREN

%%

description: e=exercise EOF {
  e
}

exercise: title=located(RAW) LBRACE questions=question* RBRACE {
  { title; questions }
}
| error {
  raise (CORE_description_CST.ParseError (
    from_lexing_position $startpos,
    from_lexing_position $endpos,
    I18N.String.parse_error
  ))
}

question: EXERCISE i=identifier d=located(exercise)
{
  Sub (i, d)
}
| EXERCISE i=identifier _d=QMARK
{
  Include (i,
           from_lexing_position $startpos(_d),
           from_lexing_position $endpos(_d))
}
| f=label LPAREN x=label RPAREN xs=located(term0)+ SEMICOLON
{
  let v = lexing_locate $startpos(f) $endpos(f) (Variable f) in
  Binding (Some x, None, app v xs)
}
| t=located(term) SEMICOLON
{
  Binding (None, None, t)
}
| i=label ty=type_ascription? EQUAL t=located(term) SEMICOLON
{
  Binding (Some i, ty, t)
}
| tys=enumeration(ty) FROM i=identifier names=enumeration(label)? SEMICOLON
{
  let names = match names with None -> All | Some n -> n in
  Import (tys, i, names)
}

term0: n=label %prec pvar
{
  Variable n
}
| r=RAW
{
  Lit (LString r)
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
| LPAREN t=structured_term RPAREN
{
  t
}
| LBRACE ts=separated_nonempty_list(SEMICOLON, located(term)) RBRACE
{
  let t = match ts with
    | [] -> assert false
    | [t] -> t
    | ts -> lexing_locate $startpos(ts) $endpos(ts) (Seq ts)
  in
  Lam (CORE_identifier.label "_", Some (TApp (TVariable "unit", [])),
       t)
}
| LPAREN error RPAREN
{
  raise (
    CORE_description_CST.ParseError (
      from_lexing_position $startpos,
      from_lexing_position $endpos,
      I18N.String.parse_error) (* FIXME: Be more informative. *)
  )
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

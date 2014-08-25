(** Parsers for description languages. *)

%{
  open AkaCST
  open Types
  open Name
  open Error
  open Position

  let error p1 p2 =
    raise (AkaError.Parse (lex_join p1 p2))

  let rec app f = function
    | [] ->
      f
    | x :: xs ->
      let f_pos = start_of_position (position f)
      and x_pos = end_of_position (position x)
      in
      app (with_pos (lex_join f_pos x_pos) (App (f, x))) xs

  let lambda pos xs e =
    let arguments =
      match xs with
        | [] -> [(Name "x", None)]
        | xs -> xs
    in
    let e = with_pos pos e in
    List.fold_left (fun e (x, a) -> with_pos pos (Lam (x, a, e)))
      e
      (List.rev arguments)

  let rec kind_of_type_parameters = Types.(function
    | [] -> KStar
    | _ :: ts -> KArrow (KStar, kind_of_type_parameters ts)
  )

%}

%start<AkaCST.t> program
%start<AkaCST.term Position.located> top_term

(** Literals. *)
%token<string> NAME UNAME TVNAME
%token<Identifier.t> IDENTIFIER
%token<int> INT
%token<char> CHAR
%token<AkaCST.template_atom list> TEMPLATE

(** Punctation. *)
%token EOF EQUAL LBRACE RBRACE LPAREN RPAREN COMMA COLON SEMICOLON PIPE DOT
%token LLPAREN RRPAREN SHARP

(** Symbols. *)
%token RARROW DRARROW UNDERSCORE QMARK

(** Keywords. *)
%token DEF DATATYPE AND TINT TUNIT TCHAR AS IMPORT DO

(** Priorities *)

%right RARROW
%nonassoc tapp

%%

program: ds=located(declaration)* EOF {
  ds
}
| error {
  error $startpos $endpos
}

top_term: t=located(term) EOF {
  t
}
| error {
  error $startpos $endpos
}

%inline define : DEF { () } | EQUAL { () }

declaration:
define
ts=type_parameters
x=name ty=ty_ascription EQUAL t=located(term) {
  let pos = Position.lex_join $startpos $endpos in
  let s = match ty with
    | None -> None
    | Some ty -> Some (TyScheme (ts, [], ty))
  in
  ValueDecl ((pos, x, s, t))
}
| define fs=separated_nonempty_list(AND, fundef) {
  RecFunDecl fs
}
| DATATYPE ts=separated_list(AND, type_definition) {
  let pos = Position.lex_join $startpos $endpos in
  TypeDecl (pos, ts)
}
| IMPORT x=IDENTIFIER {
  Import x
}

fundef:
ts=type_parameters
x=name
xs=binding+
rty=ty_ascription
EQUAL t=located(term) {
  let pos = Position.lex_join $startpos $endpos in
  let body = match rty with
    | None -> value t
    | Some rty -> TypeConstraint (t, rty)
  in
  (pos, x, None, lambda pos xs body)
}

type_definition:
x=tname ts=type_parameters EQUAL datadef=datatype_definition {
  let pos = Position.lex_join $startpos $endpos in
  let kind = kind_of_type_parameters ts in
  let rty = TyApp (pos, x, List.map (fun t -> TyVar (pos, t)) ts) in
  TypeDef (pos, kind, x, datadef ts rty)
}

datatype_definition: PIPE? ds=separated_list(PIPE, datacon_definition) {
  fun ts rty -> DAlgebraic (List.map (fun d -> d ts rty) ds)
}
| rdt=record_type
{
  fun ts _ -> DRecordType (ts, rdt)
}

datacon_definition: k=dname tys=datacon_parameters {
  fun ts rty ->
    let pos = Position.lex_join $startpos $endpos in
    (pos, k, ts, Types.ntyarrow pos tys rty)
}

datacon_parameters:
/* empty */ {
  []
}
| LPAREN ts=separated_list(COMMA, mltype) RPAREN {
  ts
}

type_parameters:
/* empty */ {
  []
}
| LPAREN ts=separated_list(COMMA, tvname) RPAREN {
  ts
}

ty_ascription: /* empty */ {
  None
}
| COLON ty=mltype {
  Some ty
}

term:
a=located(term) b=located(term0) {
  App (a, b)
}
| t=located(term) QMARK LBRACE
  PIPE? bs=separated_list(PIPE, located(branch))
  RBRACE
{
  Case (t, bs)
}
| t=term0 {
  t
}

term0:
  LBRACE xs=binding* DRARROW t=term RBRACE {
  let pos = Position.lex_join $startpos $endpos in
  Position.value (lambda pos xs t)
}
| DO LBRACE t=term RBRACE {
  let pos = Position.lex_join $startpos $endpos in
  Position.value (lambda pos [] t)
}
| x=INT {
  Lit  (LInt x)
}
| l=name {
  Variable l
}
| LBRACE k=dname ts=located(term0)+ RBRACE {
  KApp (k, ts)
}
| k=dname {
  KApp (k, [])
}
| LPAREN t=term RPAREN {
  t
}
| t=located(term0) SHARP f=lname {
  Field (t, f)
}
| LBRACE rs=separated_nonempty_list(SEMICOLON, record_binding) RBRACE {
  Record rs
}
| t=TEMPLATE {
  Template t
}
| LLPAREN ts=list(terminated(located(term), DOT)) RRPAREN
{
  Template (List.map (fun t -> Code t) ts)
}

record_binding: f=lname EQUAL t=located(term) {
  RecordBinding (f, t)
}

binding: l=name {
  (l, None)
}
| LPAREN l=name COLON ty=mltype RPAREN {
  (l, Some ty)
}

branch: p=located(pattern) DRARROW t=located(term)
{
  Branch (p, t)
}

pattern:
x=name
{
  PVar x
}
| UNDERSCORE
{
  PWildcard
}
| p=located(pattern) AS x=name
{
  PAlias (x, p)
}
| LPAREN p=located(pattern) COLON ty=mltype RPAREN
{
  PTypeConstraint (p, ty)
}
| LPAREN RPAREN
{
  PLiteral (LUnit)
}
| x=INT
{
  PLiteral (LInt x)
}
| x=CHAR
{
  PLiteral (LChar x)
}
| k=UNAME
{
  PData (DName k, [])
}
| LBRACE k=UNAME ps=located(pattern)+ RBRACE
{
  PData (DName k, ps)
}

mltype: x=tvname
{
  TyVar (lex_join $startpos $endpos, x)
}
| ity=mltype RARROW oty=mltype
{
  let pos = lex_join $startpos $endpos in
  TyApp (pos, TName "->", [ ity; oty ])
}
| t=mldatatype2
{
  t
}

mldatatype2:
 LPAREN t=mltype RPAREN {
  t
}
| t=mldatatype {
  t
}

mldatatype: TINT
{
  let pos = lex_join $startpos $endpos in
  TyApp (pos, TName "int", [])
}
| TCHAR
{
  let pos = lex_join $startpos $endpos in
  TyApp (pos, TName "char", [])
}
| TUNIT
{
  let pos = lex_join $startpos $endpos in
  TyApp (pos, TName "unit",  [])
}
| t=tname
{
  let pos = lex_join $startpos $endpos in
  TyApp (pos, t, [])
}
| t=tname ty=mltype %prec tapp
{
  let pos = lex_join $startpos $endpos in
  TyApp (pos, t, [ty])
}
| t=tname
  LPAREN ty=mltype COMMA tys=separated_nonempty_list(COMMA, mltype) RPAREN
{
  let pos = lex_join $startpos $endpos in
  TyApp (pos, t, ty :: tys)
}

record_type: LBRACE b=separated_list(SEMICOLON, label_type_declaration) RBRACE
{
  b
}

label_type_declaration: l=lname COLON t=mltype
{
  let pos = lex_join $startpos $endpos in
  (pos, l, t)
}

%inline lname: x=NAME
{
  LName x
}

%inline tname: x=NAME
{
  TName x
}

%inline dname: x=UNAME
{
  DName x
}

%inline tvname: x=TVNAME
{
  TName x
}

%inline name: n=NAME {
  Name n
}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}

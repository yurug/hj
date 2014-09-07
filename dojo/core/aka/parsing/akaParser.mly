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

  let rec apply t = function
    | [] -> t
    | a :: ts -> apply (with_pos (position t) (App (t, a))) ts

%}

%start<AkaCST.t> program
%start<AkaCST.term Position.located> top_term

(** Literals. *)
%token<string> NAME UNAME TVNAME
%token<Identifier.t> IDENTIFIER
%token<int> INT
%token<string> LSTRING
%token<char> CHAR
%token<AkaCST.template_atom list> TEMPLATE

(** Punctation. *)
%token EOF EQUAL LBRACE RBRACE LPAREN RPAREN COMMA COLON SEMICOLON PIPE DOT
%token LLPAREN RRPAREN

(** Symbols. *)
%token RARROW DRARROW UNDERSCORE QMARK

(** Keywords. *)
%token DEF DATATYPE AND TINT TUNIT TCHAR AS IMPORT DO BEGIN END WITH AKA
%token EXTERNAL

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
| IMPORT id=IDENTIFIER {
  Import id
}
| AKA LBRACE t=located(term) RBRACE {
  let pos = Position.lex_join $startpos $endpos in
  ValueDecl ((pos, Name "__questions__", None,
              with_pos pos (Lam (Name "__target__", None, t))))
}
| EXTERNAL
  ts=type_parameters
  x=name COLON ty=mltype
{
  let pos = Position.lex_join $startpos $endpos in
  External (pos, x, TyScheme (ts, [], ty))
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
  match value a with
    | KApp (k, ts) ->
      KApp (k, ts @ [ b ])
    | _ ->
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
| BEGIN LPAREN t0=located(term) RPAREN
  ts=separated_nonempty_list(WITH, located(term_sequence_template))
  END
{
  value (apply t0 ts)
}
| DO LBRACE t=term RBRACE {
  let pos = Position.lex_join $startpos $endpos in
  Position.value (lambda pos [] t)
}
| x=INT {
  Lit  (LInt x)
}
| x=LSTRING {
  Lit  (LString x)
}
| l=name {
  Variable l
}
| k=dname {
  KApp (k, [])
}
| LPAREN t=term RPAREN {
  t
}
| t=located(term0) DOT f=lname {
  Field (t, f)
}
| LBRACE
  rs=record_bindings
  RBRACE {
  Record rs
}
| t=TEMPLATE {
  Template t
}
| LLPAREN ts=term_sequence_template RRPAREN
{
  ts
}

%inline term_sequence_template: ts=term_sequence {
  Template ts
}

term_sequence:
  t=located(term) SEMICOLON?
{
  [Code t]
}
| t=located(term) SEMICOLON ts=term_sequence {
  Code t :: ts
}

record_bindings:
r=record_binding SEMICOLON? {
  [r]
}
| r=record_binding SEMICOLON rs=record_bindings {
  r :: rs
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
 LPAREN p=located(pattern) RPAREN AS x=name
{
  PAlias (x, p)
}
| LPAREN p=located(pattern) COLON ty=mltype RPAREN
{
  PTypeConstraint (p, ty)
}
| k=UNAME ps=located(pattern0)+
{
  PData (DName k, ps)
}
| p=pattern0 {
  p
}

pattern0:
LPAREN p=pattern RPAREN {
  p
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
| x=name
{
  PVar x
}
| UNDERSCORE
{
  PWildcard
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

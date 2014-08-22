(** Parsers for description languages. *)

%{
  open AkaCST
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

%}

%start<AkaCST.t> program

(** Literals. *)
%token<string> NAME
%token<int> INT

(** Punctation. *)
%token EOF EQUAL

(** Keywords. *)
%token DEF

(** Priorities *)

%%

program: ds=declaration* EOF {
  ds
}
| error {
  error $startpos $endpos
}

declaration:
DEF x=label ty=ty_ascription EQUAL t=located(term) {
  ValueDecl (x, ty, t)
}

ty_ascription: /* empty */ {
  None
}

label: n=NAME {
  Identifier.label n
}

term: x=INT {
  Lit  (LInt x)
}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}

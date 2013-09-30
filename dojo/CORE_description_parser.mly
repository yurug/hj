(** Parsers for description languages. *)

%{
  open CORE_description_CST

  let merge c q1 q2 =
    match q1, q2 with
      | Compose (c1, qs1), Compose (c2, qs2) when c1 = c2 && c = c1 ->
        Compose (c1, qs1 @ qs2)
      | Compose (c1, qs), q when c1 = c ->
        Compose (c1, qs @ [ q ])
      | q, Compose (c2, qs) when c2 = c ->
        Compose (c2, q :: qs)
      | q1, q2 ->
        Compose (c, [q1; q2])

%}

%start<CORE_description_CST.exercise> description

(** Literals. *)
%token<string> ID
%token<string> RAW

(** Punctation. *)
%token EOF LPAREN RPAREN LBRACKET RBRACKET QMARK

(** Operators *)
%token THEN ORELSE CHECK

(** Priorities *)
%right RAW
%right ORELSE
%right THEN

%%

description: e=exercise EOF {
  e
}

exercise: title=located(RAW) questions=questions {
  { title; questions }
}
| error {
  raise (CORE_description_CST.ParseError (
    $startpos,
    $endpos,
    I18N.String.parse_error
  ))
}

questions: q1=questions THEN q2=questions {
  merge Seq q1 q2
}
| q1=questions ORELSE q2=questions {
  merge Par q1 q2
}
| LPAREN q=questions RPAREN {
  q
}
| statement=located(RAW) qs=questions {
  Statement (statement, qs)
}
| CHECK i=identifier {
  Checkpoint i
}
| LBRACKET i=identifier d=located(exercise)? RBRACKET {
  Sub (i, d)
}
| LBRACKET i=identifier _d=QMARK RBRACKET {
  Include (i,
           from_lexing_position $startpos(_d),
           from_lexing_position $endpos(_d))
}

%inline identifier: id=located(ID) {
  id
}

%inline located(X): x=X {
  locate $startpos $endpos x
}

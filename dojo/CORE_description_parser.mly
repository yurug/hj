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

%start<CORE_description_CST.questions> questions_description

(** Literals. *)
%token<string> ID
%token<string> RAW

(** Punctation. *)
%token EOF LPAREN RPAREN

(** Operators *)
%token THEN ORELSE

(** Priorities *)
%right ORELSE
%right THEN

%%

questions_description: qs=questions EOF {
  qs
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
| q = question {
  Single q
}

question: x=identifier {
  Question (x, None)
}
| x=identifier qdef=question_definition {
  Question (x, Some qdef)
}

%inline identifier: id=located(ID) {
  id
}

question_definition: statement=located(RAW) {
  { statement }
}

%inline located(X): x=X {
  locate $startpos(x) $endpos(x) x
}

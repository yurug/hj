open Position
open Name
open XAST
open Types
open ElaborationExceptions

type t = {
  values       : (tnames * binding) list;
  types        : (tname * (Types.kind * type_definition)) list;
  classes      : (tname * class_definition) list;
  labels       : (lname * (tnames * Types.t * tname)) list;
}

let empty = { values = []; types = []; classes = []; labels = [] }

let values env = env.values

let lookup pos x env =
  try
    List.find (fun (_, (x', _)) -> x = x') env.values
  with Not_found -> raise (UnboundIdentifier (pos, x))

let bind_scheme x ts ty env =
  { env with values = (ts, (x, ty)) :: env.values }

let bind_simple x ty env =
  bind_scheme x [] ty env

let bind_type t kind tdef env =
  { env with types = (t, (kind, tdef)) :: env.types }

let lookup_type pos t env =
  try
    List.assoc t env.types
  with Not_found ->
    raise (UnboundTypeVariable (pos, t))

let lookup_type_kind pos t env =
  fst (lookup_type pos t env)

let lookup_type_definition pos t env =
  snd (lookup_type pos t env)

let lookup_class pos k env =
  try
    List.assoc k env.classes
  with Not_found -> raise (UnboundClass (pos, k))

let bind_class k c env =
  try
    let pos = c.class_position in
    ignore (lookup_class pos k env);
    raise (AlreadyDefinedClass (pos, k))
  with UnboundClass _ ->
    { env with classes = (k, c) :: env.classes }

let lookup_superclasses pos k env =
  (lookup_class pos k env).superclasses

let is_superclass pos k1 k2 env =
  (*<corrige>*)
  let rec search k2 k1 =
    k1 = k2 || List.exists (search k2) (lookup_superclasses pos k1 env)
  in
  search k2 k1
  (*</corrige>*)
  (*<sujet>
  (* Student! This is your job! *)
  true
  </sujet>*)

let bind_type_variable t env =
  bind_type t KStar (TypeDef (dummy, KStar, t, DAlgebraic [])) env

let labels_of rtcon env =
  let p (_, (_, _, rtcon')) = rtcon = rtcon' in
  List.(fst (split (filter p env.labels)))

let lookup_label pos l env =
  try
    List.assoc l env.labels
  with Not_found ->
    raise (UnboundLabel (pos, l))

let bind_label pos l ts ty rtcon env =
  try
    ignore (lookup_label pos l env);
    raise (LabelAlreadyTaken (pos, l))
  with UnboundLabel _ ->
    { env with labels = (l, (ts, ty, rtcon)) :: env.labels }

let initial =
  let primitive_type t k = TypeDef (dummy, k, t, DAlgebraic []) in
  List.fold_left (fun env (t, k) ->
    bind_type t k (primitive_type t k) env
  ) empty [
    (TName "->", KArrow (KStar, KArrow (KStar, KStar)));
    (TName "int", KStar);
    (TName "char", KStar);
    (TName "unit", KStar)
  ]

(*<corrige>*)

type namespace = LetBound | Overloaded

let reserved = Hashtbl.create 13

let reserve kind pos i =
  try
    if Hashtbl.find reserved i <> kind then
      raise (OverloadedSymbolCannotBeBound (pos, i))
  with Not_found ->
    Hashtbl.add reserved i kind

let reserve_overloaded_name = reserve Overloaded

let reserve_let_bound_name = reserve LetBound

(*</corrige>*)

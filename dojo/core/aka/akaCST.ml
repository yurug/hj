(* -*- tuareg -*- *)

(** Concrete syntax trees. *)
open Lwt
open Error
open Position
open Identifier
open Name

type t = declaration located list

and declaration =
  | External     of position * name * ty_scheme
  | ValueDecl    of value_definition
  | RecFunDecl   of value_definition list
  | TypeDecl     of position * type_definition list
  | Import       of identifier

and value_definition =
    position * name * ty_scheme option * term'

and term =
  | Lit of literal
  | Template of template

  | Variable of name
  | Lam of name * mltype option * term'
  | App of term' * term'
  | KApp of dname * term' list
  | TypeConstraint of term' * mltype
  | Record of record_binding list
  | Field of term' * lname
  | Case of term' * branch' list

and branch =
  | Branch of pattern' * term'

and branch' = branch located

and pattern =
  | PVar of name
  | PWildcard
  | PAlias of name * pattern'
  | PTypeConstraint of pattern' * mltype
  | PLiteral of literal
  | PData of dname * pattern' list
  | PAnd of pattern' list
  | POr of pattern' list

and pattern' = pattern located

and record_binding =
  | RecordBinding of lname * term'

and template = template_atom list

and template_atom =
  | Raw  of string located
  | Code of term'

  (** Should not occur in final CST. This constructor is used in
      intermediate trees during multi-level parsing. *)
  | RawCode of string located

and term' = term located

and literal =
  | LUnit
  | LChar   of char
  | LInt    of int
  | LFloat  of float
  | LString of string

and ty_scheme = Types.scheme

and mltype = Types.t

and type_definition =
  | TypeDef of position * mltypekind * tname * datatype_definition
  | ExternalType of position * tnames * tname

and datatype_definition =
  | DAlgebraic  of (position * dname * tnames * mltype) list
  | DRecordType of tnames * (position * lname * mltype) list

and tnames = tname list

and mltypekind = Types.kind

deriving (Json)

let loader : (identifier -> [> `OK of t | `KO of identifier ] Lwt.t) ref =
  ref (fun _ -> assert false)

let set_loader = ( := ) loader

let load p = !loader (identifier_of_path p)

type 'a with_raw = string * 'a

let with_raw s cst = (s, cst)

let raw = fst

let data = snd

let rec equivalent_terms t1 t2 =
  match t1, t2 with
    | Lit l1, Lit l2 ->
      l1 = l2
    | Variable x1, Variable x2 ->
      x1 = x2
    | Lam (x1, ty1, t1), Lam (x2, ty2, t2) ->
      x1 = x2 && ty1 = ty2 && equivalent_terms' t1 t2
    | App (a1, b1), App (a2, b2) ->
      equivalent_terms' a1 a2 && equivalent_terms' b1 b2
    | _, _ ->
      false

and equivalent_terms' t1 t2 = Position.(equivalent_terms (value t1) (value t2))

module I = IAST
module T = Types.ImplicitTyping

let to_ast : identifier -> t -> IAST.program Lwt.t = fun module_name program ->

  let fresh_record_name =
    let c = ref 0 in
    fun () -> incr c; Name ("_record" ^ string_of_int !c)
  in

  let load_module =
    let loaded_modules = ref Identifier.Set.empty in
    let loaded m = Identifier.Set.mem m !loaded_modules in
    let mark m = loaded_modules := Identifier.Set.add m !loaded_modules in
    let load_module m =
      let path = path_of_identifier m in
      if loaded m then return (`OK []) else (
        mark m;
        load path
      )
    in
    load_module
  in
  let rec t ds =
    lwt ds = Lwt_list.map_s declaration' ds in
    return (List.flatten ds)

  and declaration' d =
    declaration (position d) (value d)

  and declaration pos = function
    | External (pos, (Name s as name), Types.TyScheme (ts, _, ty)) ->
      return [
        I.(BDefinition (ExternalValue (pos, ts, (name, Some ty), s)))
      ]

    | ValueDecl vdef ->
      return [
        I.(BDefinition (BindValue (pos, value_definition vdef)))
      ]

    | RecFunDecl (vdefs) ->
      return [
        I.(BDefinition (
          BindRecValue (pos, List.(flatten (map value_definition vdefs)))
        ))
      ]

    | TypeDecl (pos, tdefs) ->
      return [
        I.BTypeDefinitions (I.TypeDefs (pos, List.map type_definition tdefs))
      ]

    | Import m ->
      load_module m >>= function
        | `OK cst -> t cst
        | `KO id ->
          (* Self inclusion is ignored.
             This is convenient for std.aka compilation. *)
          if Identifier.compare id module_name = 0 then
            return []
          else
            raise_lwt (AkaError.LoadModule m)

  and value_definition (pos, l, a, t) =
      let ts, ty = destruct_tyscheme_option a in
      [I.ValueDef (pos, ts, [], vbinding pos l ty, term' t)]

  and type_definition = function
    | TypeDef (pos, k, a, datadef) ->
      I.TypeDef (pos, k, a, datatype_definition datadef)
    | ExternalType (pos, ts, (TName s as a)) ->
      I.ExternalType (pos, ts, a, s)

  and datatype_definition = function
    | DAlgebraic (kdefs) ->
      I.DAlgebraic (kdefs)
    | DRecordType (ts, ls) ->
      I.DRecordType (ts, ls)

  and vbinding pos n a =
    I.binding (Position.start_of_position pos) n (option_ty a)

  and term pos : term -> _ = function
    | Lit l ->
      I.EPrimitive (pos, literal l)

    | Variable x ->
      I.EVar (pos, x, None)

    | Lam (x, a, t) ->
      I.ELambda (pos, vbinding pos x a, term' t)

    | App (a, b) ->
      I.EApp (pos, term' a, term' b)

    | KApp (k, ts) ->
      I.EDCon (pos, k, None, List.map term' ts)

    | Template t ->
      template pos t

    | TypeConstraint (t, ty) ->
      I.ETypeConstraint (pos, term' t, ty)

    | Field (t, f) ->
      I.ERecordAccess (pos, term' t, f)

    | Record rs ->
      I.ERecordCon (pos, fresh_record_name (), None, List.map record_binding rs)

    | Case (t, bs) ->
      I.EMatch (pos, term' t, List.map branch' bs)

  and template pos = function
    | [] -> I.EDCon (pos, DName "TNil", None, [])
    | Raw s :: t ->
      I.(EDCon (pos,
                DName "TAtom", None, [
                  EPrimitive (position s, PStringConstant (value s));
                  template pos t
                ]))
    | Code c :: t ->
      I.(EDCon (pos, DName "TCode", None, [ term' c; template pos t ]))
    | RawCode _ :: _ ->
      assert false (* Thanks to multi-level parsing. *)

  and branch pos (Branch (p, t)) =
    I.Branch (pos, pattern' p, term' t)

  and branch' b = branch (position b) (value b)

  and pattern' p = pattern (position p) (value p)

  and pattern pos = function
    | PVar x -> I.PVar (pos, x)
    | PWildcard -> I.PWildcard pos
    | PAlias (l, p) -> I.PAlias (pos, l, pattern' p)
    | PTypeConstraint (p, ty) -> I.PTypeConstraint (pos, pattern' p, ty)
    | PLiteral l -> I.PPrimitive (pos, literal l)
    | PData (k, ps) -> I.PData (pos, k, None, List.map pattern' ps)
    | PAnd ps -> I.PAnd (pos, List.map pattern' ps)
    | POr ps -> I.POr (pos, List.map pattern' ps)

  and record_binding (RecordBinding (l, t)) =
    I.RecordBinding (l, term' t)

  and term' t = term (position t) (value t)

  and destruct_tyscheme_option = function
    | None -> ([], None)
    | Some (Types.TyScheme (ts, _, ty)) -> (List.map type_variable ts, Some ty)

  and literal = function
    | LInt x -> I.PIntegerConstant x
    | LFloat f -> failwith "TODO"
    | LString s -> I.PStringConstant s
    | LUnit -> I.PUnit
    | LChar c -> I.PCharConstant c

  and option_ty = function
    | None -> None
    | Some t -> Some (ty t)

  and ty t = t

  and type_variable v = v
  in
  t (Position.(with_pos dummy (Import (identifier_of_string "std")))
     :: program)

let rec substitute_term_in_term f =
  let rec term' t = f (Position.map term t)

  and term = function
    | Template t -> Template (List.map template_atom t)
    | Lam (x, a, t) -> Lam (x, a, term' t)
    | App (a, b) -> App (term' a, term' b)
    | KApp (k, ts) -> KApp (k, List.map term' ts)
    | TypeConstraint (t, ty) -> TypeConstraint (term' t, ty)
    | Record rs -> Record (List.map record_binding rs)
    | Field (t, l) -> Field (term' t, l)
    | Case (t, bs) -> Case (term' t, List.map branch' bs)
    | (Lit _ | Variable _ as x) -> x

  and template_atom = function
    | Raw s -> Raw s
    | RawCode s -> RawCode s
    | Code t -> Code (term' t)

  and branch (Branch (p, t)) =
    Branch (p, term' t)

  and branch' b = Position.map branch b

  and record_binding (RecordBinding (l, t)) = RecordBinding (l, term' t)
  in
  term'

let substitute_term_in_program f =
  let rec t fs = List.map declaration' fs

  and declaration' d = Position.map declaration d

  and declaration = function
    | ValueDecl vdef -> ValueDecl (value_definition vdef)
    | RecFunDecl vdefs -> RecFunDecl (List.map value_definition vdefs)
    | t -> t

  and value_definition (pos, x, s, t) =
    (pos, x, s, substitute_term_in_term f t)

  in
  t

let substitute_template f = Position.map (function
  | Template t -> Template (List.map f t)
  | u -> u
)

let substitute_template_in_program f =
  substitute_term_in_program (substitute_template f)

let substitute_template_in_term f =
  substitute_term_in_term (substitute_template f)

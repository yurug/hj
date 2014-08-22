open String
open Name
open XAST
open Types
open Positions
open ElaborationErrors
open ElaborationExceptions
open ElaborationEnvironment

let string_of_type ty      = ASTio.(XAST.(to_string pprint_ml_type ty))

(*<corrige>*)
let fresh_instance_name =
  let r = ref 0 in
  fun () -> incr r; Name ("instance_" ^ string_of_int !r)
(*</corrige>*)

let rec program p = handle_error List.(fun () ->
  flatten (fst (Misc.list_foldmap block ElaborationEnvironment.initial p))
)

and block env = function
  | BTypeDefinitions ts ->
    let env = type_definitions env ts in
    ([BTypeDefinitions ts], env)

  | BDefinition d ->
    let d, env = value_binding env d in
    ([BDefinition d], env)

  | BClassDefinition c ->
    (*<corrige>*)
    let env = bind_class c.class_name c env in
    let ms = class_members c in
    let class_defs = class_definitions c ms in
    let env = env_of_bindings env class_defs in
    let defs =
      BTypeDefinitions (class_type_definitions c ms)
      :: [BDefinition class_defs]
    in
    (defs, env)
    (*</corrige>*)
    (*<sujet>
    (** Class definitions are ignored. Student! This is your job! *)
    ([], env)
    </sujet>*)

  | BInstanceDefinitions is ->
    (*<corrige>*)
    let (is, env) = instance_definitions env is in
    (List.map (fun x -> BDefinition x) is, env)
    (*</corrige>*)
    (*<sujet>
    (** Instance definitions are ignored. Student! This is your job! *)
    ([], env)
    </sujet>*)

and type_definitions env (TypeDefs (_, tdefs)) =
  let env = List.fold_left env_of_type_definition env tdefs in
  List.fold_left type_definition env tdefs

and env_of_type_definition env = function
  | (TypeDef (pos, kind, t, _)) as tdef ->
    bind_type t kind tdef env

  | (ExternalType (p, ts, t, os)) as tdef ->
    bind_type t (kind_of_arity (List.length ts)) tdef env

and type_definition env = function
  | TypeDef (pos, _, t, dt) -> datatype_definition t env dt
  | ExternalType (p, ts, t, os) -> env

and datatype_definition t env = function
  | DAlgebraic ds ->
    List.fold_left algebraic_dataconstructor env ds
  | DRecordType (ts, ltys) ->
    List.fold_left (label_type ts t) env ltys

and label_type ts rtcon env (pos, l, ty) =
  let env' = List.fold_left (fun env x -> bind_type_variable x env) env ts in
  check_wf_type env' KStar ty;
  bind_label pos l ts ty rtcon env

and algebraic_dataconstructor env (_, DName k, ts, kty) =
  check_wf_scheme env ts kty;
  bind_scheme (Name k) ts kty env

and introduce_type_parameters env ts =
  List.fold_left (fun env t -> bind_type_variable t env) env ts

and check_wf_scheme env ts ty =
  check_wf_type (introduce_type_parameters env ts) KStar ty

and check_wf_type env xkind = function
  | TyVar (pos, t) ->
    let tkind = lookup_type_kind pos t env in
    check_equivalent_kind pos xkind tkind

  | TyApp (pos, t, tys) ->
    let kt = lookup_type_kind pos t env in
    check_type_constructor_application pos env kt tys

and check_type_constructor_application pos env k tys =
  match tys, k with
  | [], KStar -> ()
  | ty :: tys, KArrow (k, ks) ->
    check_wf_type env k ty;
    check_type_constructor_application pos env ks tys
  | _ ->
    raise (IllKindedType pos)

and check_equivalent_kind pos k1 k2 =
  match k1, k2 with
    | KStar, KStar -> ()
    | KArrow (k1, k2), KArrow (k1', k2') ->
      check_equivalent_kind pos k1 k1';
      check_equivalent_kind pos k2 k2'
    | _ ->
      raise (IncompatibleKinds (pos, k1, k2))

and env_of_bindings env cdefs = List.(
  (function
    | BindValue (_, vs)
    | BindRecValue (_, vs) ->
      fold_left (fun env (ValueDef (_, ts, _, (x, ty), _)) ->
        bind_scheme x ts ty env
      ) env vs
    | ExternalValue (_, ts, (x, ty), _) ->
      bind_scheme x ts ty env
  ) cdefs
)

and check_equal_types pos ty1 ty2 =
  if not (equivalent ty1 ty2) then
    raise (IncompatibleTypes (pos, ty1, ty2))

and type_application pos env x tys =
  List.iter (check_wf_type env KStar) tys;
  let (ts, (_, ty)) = lookup pos x env in
  try
    substitute (List.combine ts tys) ty
  with _ ->
    raise (InvalidTypeApplication pos)

and expression env = function
  | EVar (pos, ((Name s) as x), tys) ->
    (*<corrige>*)
    let ty = type_application pos env x tys in
    pass_dictionary pos env ty (EVar (pos, x, tys))
    (*</corrige>*)
    (*<sujet>
    (EVar (pos, x, tys), type_application pos env x tys)
    </sujet>*)

  | ELambda (pos, ((x, aty) as b), e') ->
    check_wf_type env KStar aty;
    let env = bind_simple x aty env in
    let (e, ty) = expression env e' in
    (ELambda (pos, b, e), ntyarrow pos [aty] ty)

  | EApp (pos, a, b) ->
    let a, a_ty = expression env a in
    let b, b_ty = expression env b in
    begin match destruct_tyarrow a_ty with
      | None ->
        raise (ApplicationToNonFunctional pos)
      | Some (ity, oty) ->
        check_equal_types pos b_ty ity;
        (EApp (pos, a, b), oty)
    end

  | EBinding (pos, vb, e) ->
    let vb, env = value_binding env vb in
    let e, ty = expression env e in
    (EBinding (pos, vb, e), ty)

  | EForall (pos, tvs, e) ->
    (** Because type abstractions are removed by [value_binding]. *)
    raise (OnlyLetsCanIntroduceTypeAbstraction pos)

  | ETypeConstraint (pos, e, xty) ->
    let e, ty = expression env e in
    check_equal_types pos ty xty;
    (e, ty)

  | EExists (_, _, e) ->
    (** Because we are explicitly typed, flexible type variables
        are useless. *)
    expression env e

  | EDCon (pos, DName x, tys, es) ->
    let ty = type_application pos env (Name x) tys in
    let (itys, oty) = destruct_ntyarrow ty in
    if List.(length itys <> length es) then
      raise (InvalidDataConstructorApplication pos)
    else
      let es =
        List.map2 (fun e xty ->
          let (e, ty) = expression env e in
          check_equal_types pos ty xty;
          e
        ) es itys
      in
      (EDCon (pos, DName x, tys, es), oty)

  | EMatch (pos, s, bs) ->
    let (s, sty) = expression env s in
    let bstys = List.map (branch env sty) bs in
    let bs = fst (List.split bstys) in
    let tys = snd (List.split bstys) in
    let ty = List.hd tys in
    List.iter (check_equal_types pos ty) (List.tl tys);
    (EMatch (pos, s, bs), ty)

  | ERecordAccess (pos, e, l) ->
    let e, ty = expression env e in
    let (ts, lty, rtcon) = lookup_label pos l env in
    let ty =
      match ty with
        | TyApp (_, r, args) ->
          if rtcon <> r then
            raise (LabelDoesNotBelong (pos, l, r, rtcon))
          else
            begin try
              let s = List.combine ts args in
              Types.substitute s lty
            with _ ->
              (** Because we only well-kinded types and only store
                  well-kinded types in the environment. *)
              assert false
            end
        | _ ->
          raise (RecordExpected (pos, ty))
    in
    (ERecordAccess (pos, e, l), ty)

  | ERecordCon (pos, n, i, []) ->
    (** We syntactically forbids empty records. *)
    assert false

  | ERecordCon (pos, n, i, rbs) ->
    let rbstys = List.map (record_binding env) rbs in
    let rec check others rty = function
      | [] ->
        begin match rty with
          | Some (_, TyApp (_, rtcon, _)) ->
            let labels = labels_of rtcon env in
            if (List.length labels <> List.length others) then
              raise (InvalidRecordConstruction pos)
          | _ -> assert false (** Because we forbid empty record. *)
        end;
        List.rev others, rty
      | (RecordBinding (l, e), ty) :: ls ->
        if List.exists (fun (RecordBinding (l', _)) -> l = l') others then
          raise (MultipleLabels (pos, l));

        let (ts, lty, rtcon) = lookup_label pos l env in
        let (s, rty) =
          match rty with
            | None ->
              let rty = TyApp (pos, rtcon, i) in
              let s =
                try
                  List.combine ts i
                with _ -> raise (InvalidRecordInstantiation pos)
              in
              (s, rty)
            | Some (s, rty) ->
              (s, rty)
        in
        check_equal_types pos ty (Types.substitute s lty);
        check (RecordBinding (l, e) :: others) (Some (s, rty)) ls
    in
    let (ls, rty) = check [] None rbstys in
    let rty = match rty with
      | None -> assert false
      | Some (_, rty) -> rty
    in
    (ERecordCon (pos, n, i, ls), rty)

  | ((EPrimitive (pos, p)) as e) ->
    (e, primitive pos p)

and primitive pos = function
  | PIntegerConstant _ ->
    TyApp (pos, TName "int", [])

  | PUnit ->
    TyApp (pos, TName "unit", [])

  | PCharConstant _ ->
    TyApp (pos, TName "char", [])

and branch env sty (Branch (pos, p, e)) =
  let denv = pattern env sty p in
  let env = concat pos env denv in
  let (e, ty) = expression env e in
  (Branch (pos, p, e), ty)

and concat pos env1 env2 =
  List.fold_left
    (fun env (_, (x, ty)) -> bind_simple x ty env)
    env1 (values env2)

and linear_bind pos env (ts, (x, ty)) =
  assert (ts = []); (** Because patterns only bind monomorphic values. *)
  try
    ignore (lookup pos x env);
    raise (NonLinearPattern pos)
  with UnboundIdentifier _ ->
    bind_simple x ty env

and join pos denv1 denv2 =
  List.fold_left (linear_bind pos) denv2 (values denv1)

and check_same_denv pos denv1 denv2 =
  List.iter (fun (ts, (x, ty)) ->
    assert (ts = []); (** Because patterns only bind monomorphic values. *)
    try
      let (_, (_, ty')) = lookup pos x denv2 in
      check_equal_types pos ty ty'
    with _ ->
      raise (PatternsMustBindSameVariables pos)
  ) (values denv1)

and pattern env xty = function
  | PVar (_, name) ->
    bind_simple name xty ElaborationEnvironment.empty

  | PWildcard _ ->
    ElaborationEnvironment.empty

  | PAlias (pos, name, p) ->
    linear_bind pos (pattern env xty p) ([], (name, xty))

  | PTypeConstraint (pos, p, pty) ->
    check_equal_types pos pty xty;
    pattern env xty p

  | PPrimitive (pos, p) ->
    check_equal_types pos (primitive pos p) xty;
    ElaborationEnvironment.empty

  | PData (pos, (DName x), tys, ps) ->
    let kty = type_application pos env (Name x) tys in
    let itys, oty = destruct_ntyarrow kty in
    if List.(length itys <> length ps) then
      raise (InvalidDataConstructorApplication pos)
    else
      let denvs = List.map2 (pattern env) itys ps in (
        check_equal_types pos oty xty;
        List.fold_left (join pos) ElaborationEnvironment.empty denvs
      )

  | PAnd (pos, ps) ->
    List.fold_left
      (join pos)
      ElaborationEnvironment.empty
      (List.map (pattern env xty) ps)

  | POr (pos, ps) ->
    let denvs = List.map (pattern env xty) ps in
    let denv = List.hd denvs in
    List.(iter (check_same_denv pos denv) (tl denvs));
    denv

and record_binding env (RecordBinding (l, e)) =
  let e, ty = expression env e in
  (RecordBinding (l, e), ty)

and value_binding env = function
  | BindValue (pos, vs) ->
    let (vs, env) = Misc.list_foldmap value_definition env vs in
    (BindValue (pos, vs), env)

  | BindRecValue (pos, vs) ->
    let env = List.fold_left value_declaration env vs in
    let (vs, _) = Misc.list_foldmap value_definition env vs in
    (BindRecValue (pos, vs), env)

  | ExternalValue (pos, ts, ((x, ty) as b), os) ->
    let env = bind_scheme x ts ty env in
    (ExternalValue (pos, ts, b, os), env)

and eforall pos ts e =
  match ts, e with
    | ts, EForall (pos, [], ((EForall _) as e)) ->
      eforall pos ts e
    | [], EForall (pos, [], e) ->
      e
    | [], EForall (pos, _, _) ->
      raise (InvalidNumberOfTypeAbstraction pos)
    | [], e ->
      e
    | x :: xs, EForall (pos, t :: ts, e) ->
      if x <> t then
        raise (SameNameInTypeAbstractionAndScheme pos);
      eforall pos xs (EForall (pos, ts, e))
    | _, _ ->
      raise (InvalidNumberOfTypeAbstraction pos)

(*<corrige>*)

and class_tname (TName k) = TName (uncapitalize k)

and class_of_tname (TName k) = TName (capitalize k)

and class_type pos k a = TyApp (pos, k, [TyVar (pos, a)])

and superclasses_as_members p =
  let pos = p.class_position in
  let (TName class_sname) = p.class_name in
  let class_sname = uncapitalize class_sname in
  let superclass_member_name (TName k) = LName (class_sname ^ "_as_" ^ k) in
  let superclass_type k = class_type pos (class_tname k) p.class_parameter in
  let superclass_member k =
    (pos, superclass_member_name k, superclass_type k)
  in
  List.map superclass_member p.superclasses

and class_members p = superclasses_as_members p @ p.class_members

and class_type_definitions p members =
  let pos = p.class_position in
  let type_name = class_tname p.class_name in
  let type_definition =
    TypeDef (pos, KArrow (KStar, KStar), type_name,
             DRecordType ([p.class_parameter], members))
  in
  TypeDefs (p.class_position, [ type_definition ])

and class_definitions p members =
  let pos = p.class_position in
  let class_type =
    class_type pos (class_tname p.class_name) p.class_parameter
  in
  let member (pos, ((LName x) as l), ty) =
    let ty = ntyarrow pos [class_type] ty in
    let t =
      EForall (pos, [p.class_parameter],
               ELambda (pos, (Name "z", class_type),
                        ERecordAccess (pos,
                                       (EVar (pos, Name "z", [])),
                                       l)))
    in
    reserve_overloaded_name pos (Name x);
    ValueDef (pos, [p.class_parameter], [], (Name x, ty), t)
  in
  BindValue (pos, List.map member members)

and instance_context_parameters i =
  let pos = i.instance_position in
  List.map (fun (ClassPredicate (k, v)) ->
    class_type pos (class_tname k) v
  ) i.instance_typing_context

and instance_index_type i =
  let pos = i.instance_position in
  TyApp (pos, i.instance_index,
         List.map (fun v -> TyVar (pos, v)) i.instance_parameters)

and instance_type i =
  TyApp (i.instance_position,
         class_tname i.instance_class_name,
         [instance_index_type i])

and instance_constructor_type i =
  ntyarrow i.instance_position (instance_context_parameters i) (instance_type i)

and instance_declaration i =
  (i.instance_parameters, (fresh_instance_name (), instance_constructor_type i))

and check_nonoverlapping_instances env i =
  let ty = instance_type i in
  List.iter (fun (ts, (_, clause)) ->
    let (_, pattern) = destruct_ntyarrow clause in
    match pattern with
      | TyApp (_, TName _, [TyVar _]) ->
        ()
      | TyApp (_, TName _, _) ->
        begin match match_type 0 ts pattern ty with
          | Some _ ->
            raise (OverlappingInstances (i.instance_position,
                                         i.instance_class_name))
          | None -> ()
        end
      | _ -> ()
  ) (values env)

and instance_definitions env = function
  | [] ->
    ([], env)
  | ids -> List.(
    let idecs = map instance_declaration ids in
    let rec_env =
      fold_left (fun env (ts, (x, ty)) -> bind_scheme x ts ty env) env idecs
    in
    let rec aux env bs idecs is =
      match idecs, is with
      | [], [] ->
        (env, List.rev bs)
      | ((ts, (x, ty)) as d) :: idecs, i :: is ->
        check_nonoverlapping_instances env i;
        let b = instance_definition rec_env env d i in
        let env = bind_scheme x ts ty env in
        aux env (b :: bs) idecs is
      | _, _ -> assert false
    in
    let (env, bs) = aux env [] idecs ids in
    ([BindRecValue (undefined_position, bs) ], env)
  )

and instance_definition rec_env env (parameters, binding) i =
  let pos = i.instance_position in
  check_canonical_typing_context pos env i.instance_typing_context;
  let c = lookup_class pos i.instance_class_name env in
  let instance_term =
    let rec aux bs t = function
      | [] ->
        t bs
      | k :: ks ->
        let b = ([], (fresh_instance_name (), k)) in
        aux (b :: bs) (fun xs -> ELambda (pos, snd b, t xs)) ks
    in
    let dictionary bs = List.(
      let bind_bs env =
        List.fold_left (fun env (ts, (x, ty)) -> bind_scheme x ts ty env) env bs
      in
      let env = bind_bs env
      and rec_env = bind_bs rec_env
      in
      let superclass_member (pos, s, t) =
        match t with
          | TyApp (_, k, _) ->
            let t = TyApp (pos, k, [instance_index_type i]) in
            let d = dictionary pos env t in
            RecordBinding (s, d)
          | _ ->
            assert false (** Because of the syntax for instance. *)
      in
      let superclass_ms = map superclass_member (superclasses_as_members c) in
      let member (pos, s, ty) (RecordBinding (_, e)) =
        let e_env = match e with
          | ELambda _ -> rec_env
          | _ -> env
        in
        let e_env =
          List.fold_left
            (fun env x -> bind_type_variable x env)
            e_env parameters
        in
        let (e, ty) = expression e_env e in
        RecordBinding (s, e)
      in
      let ms = map2 member c.class_members i.instance_members in
      ERecordCon (pos, Name "", [instance_index_type i], superclass_ms @ ms)
    )
    in
    EForall (pos, parameters,
             aux [] dictionary (List.rev (instance_context_parameters i))
    )
  in
  ValueDef (pos, parameters, [], binding, instance_term)

and dictionary pos env0 ty =
  let rec search level ty env =
    match env with
    | [] -> raise Not_found

    | (ts, (z, clause)) :: env ->
      match reduce level ts clause ty with
        | Some (tys, instantiated_clause, subgoals) ->
          (try
            List.fold_left
              (fun t ty -> EApp (pos, t, search (level + 1) ty (values env0)))
              (EVar (pos, z, tys))
              subgoals
           with Not_found -> search level ty env)
        | None ->
          search level ty env
  in
  try
    search 0 ty (values env0)
  with Not_found -> raise (CannotElaborateDictionary (pos, ty))

and reduce level ts clause ty =
  let (_, pattern) = destruct_ntyarrow clause in
  match match_type level ts pattern ty with
    | Some subst ->
      let iclause = substitute subst clause in
      let (subgoals, _) = destruct_ntyarrow iclause in
      let tys = snd (List.split subst) in
      Some (tys, iclause, subgoals)
    | None ->
      None

and match_type level tvs pattern goal =
  let r = match tvs, pattern, goal with
    | ([v],
       TyApp (_, k, [ TyVar (_, x) ]),
       TyApp (_, k', [ ty ])) when v = x && k = k' ->
      Some [ (x, ty) ]

    | ([], TyApp (_, k, [ TyVar (_, x) ]),
       TyApp (_, k', [ TyVar (_, y) ])) when x = y && k = k' ->
      Some []

    | (tvs, TyApp (_, k, [ TyApp (_, g, _) ]),
       TyApp (_, k', [ TyApp (_, g', tys) ])) when g = g' && k = k' ->
      Some (List.combine tvs tys)

    | _ ->
      None
  in
(*  Printf.eprintf "  %s[%d] %s <?= %s -> %B\n"
    (String.make (level * 2) ' ')
    (List.length tvs)
    (string_of_type pattern)
    (string_of_type goal)
    (r <> None);*)
  r

and check_canonical_typing_context pos env context =
  let sorted_context =
    List.sort (fun (ClassPredicate (_, p1)) (ClassPredicate (_, p2)) ->
      Pervasives.compare p1 p2)
      context
  in
  let rec aux groups cs =
    match cs, groups with
      | [], _ -> groups
      | x :: xs, [] -> aux [[x]] xs
      | (ClassPredicate (k1, a1)) :: xs,
        ((ClassPredicate (k2, a2)) :: cs1) :: g when a1 = a2 ->
        aux
          ((ClassPredicate (k1, a1) :: ClassPredicate (k2, a2) :: cs1) :: g)
          xs
      | x :: xs, cs :: groups ->
        aux ([x] :: cs :: groups) xs
  in
  (** Class predicates are grouped by the same parameter. *)
  let groups = aux [] sorted_context in
  (** Inside a group, all predicates must be unrelated. *)
  let check_unrelated k1 k2 =
    if (is_superclass pos k1 k2 env) || (is_superclass pos k2 k1 env) then
      raise (TheseTwoClassesMustNotBeInTheSameContext (pos, k1, k2))
  in
  let rec check_group = function
    | [] -> ()
    | (ClassPredicate (k1, _)) :: g ->
      List.iter (fun (ClassPredicate (k2, _)) -> check_unrelated k1 k2) g;
      check_group g
  in
  List.iter check_group groups

and pass_dictionary pos env ity e =
  let (dicts, ty) = canonical_to_simple_type pos env ity in
  let e =
    List.fold_left (fun e d ->
      let q = dictionary pos env d in
      EApp (pos, e, q)
    ) e (List.rev dicts)
  in
  (e, ty)

and canonical_to_simple_type pos env ty =
  let (inputs, output) = destruct_ntyarrow ty in
  let dicts, regulars = List.partition (is_class_predicate env) inputs in
  (dicts, ntyarrow pos regulars output)

and is_class_predicate env = function
  | TyApp (pos, k, _) ->
    (try ignore (lookup_class pos (class_of_tname k) env); true with _ -> false)
  | _ ->
    false

and is_simple env ty =
  fst (canonical_to_simple_type undefined_position env ty) = []

and extract_dictionary_context pos env ps e =
  List.fold_left (fun (tyctx, ctx, env) (ClassPredicate (k, t)) ->
    let ty = TyApp (pos, class_tname k, [TyVar (pos, t)]) in
    let (x, ty) as b = (fresh_instance_name (), ty) in
    let tyctx x = tyctx (ntyarrow pos [ty] x) in
    let ctx x = ctx (ELambda (pos, b, x)) in
    let env = bind_simple x ty env in
    (tyctx, ctx, env)
  ) ((fun x -> x), (fun x -> x), env) (List.rev ps)

and extract_dictionary_type_context pos ps =
  List.fold_left (fun tyctx (ClassPredicate (k, t)) ->
    let ty = TyApp (pos, class_tname k, [TyVar (pos, t)]) in
    let tyctx x = tyctx (ntyarrow pos [ty] x) in
    tyctx
  ) (fun x -> x) (List.rev ps)

and value_definition env (ValueDef (pos, ts, ps, (x, xty), e)) =
  reserve_let_bound_name pos x;
  let env' = introduce_type_parameters env ts in
  check_wf_scheme env ts xty;

  if is_value_form e then begin
    let e = eforall pos ts e in
    let (tyctx, ctx, env_e) = extract_dictionary_context pos env' ps e in
    let e, ty = expression env_e e in
    check_equal_types pos xty ty;
    check_type_scheme pos env (TyScheme (ts, ps, xty));
    let b = (x, tyctx ty) in
    (ValueDef (pos, ts, [], b, EForall (pos, ts, ctx e)),
     bind_scheme x ts (tyctx ty) env)
  end else begin
    if ts <> [] then
      raise (ValueRestriction pos)
    else if ps <> [] then
      raise (InvalidOverloading pos)
    else
      let e = eforall pos [] e in
      let e, ty = expression env' e in
      let b = (x, ty) in
      check_equal_types pos xty ty;
      (ValueDef (pos, [], [], b, e), bind_simple x ty env)
  end

and value_declaration env (ValueDef (pos, ts, ps, (x, ty), e)) =
  let tyctx = extract_dictionary_type_context pos ps in
  bind_scheme x ts (tyctx ty) env

(*</corrige>*)

(*<sujet>
and value_definition env (ValueDef (pos, ts, ps, (x, xty), e)) =
  let env' = introduce_type_parameters env ts in
  check_wf_scheme env ts xty;

  if is_value_form e then begin
    let e = eforall pos ts e in
    let e, ty = expression env' e in
    let b = (x, ty) in
    check_equal_types pos xty ty;
    (ValueDef (pos, ts, [], b, EForall (pos, ts, e)),
     bind_scheme x ts ty env)
  end else begin
    if ts <> [] then
      raise (ValueRestriction pos)
    else
      let e = eforall pos [] e in
      let e, ty = expression env' e in
      let b = (x, ty) in
      check_equal_types pos xty ty;
      (ValueDef (pos, [], [], b, e), bind_simple x ty env)
  end

and value_declaration env (ValueDef (pos, ts, ps, (x, ty), e)) =
  bind_scheme x ts ty env

</sujet>*)

and is_value_form = function
  | EVar _
  | ELambda _
  | EPrimitive _ ->
    true
  | EDCon (_, _, _, es) ->
    List.for_all is_value_form es
  | ERecordCon (_, _, _, rbs) ->
    List.for_all (fun (RecordBinding (_, e)) -> is_value_form e) rbs
  | EExists (_, _, t)
  | ETypeConstraint (_, t, _)
  | EForall (_, _, t) ->
    is_value_form t
  | _ ->
    false

(*<corrige>*)

and check_type_scheme pos env (TyScheme (ts, c, ty)) =
  Misc.StringSet.(
  let vs =
    List.fold_left (fun vs (ClassPredicate (k, TName v)) ->
      add v vs
    ) empty c
  in
  let tyvs = InternalizeTypes.variables_of_typ ty in
  if not (subset vs tyvs) then
    raise (InvalidOverloading pos)
  );
  check_canonical_typing_context pos env c

(*</corrige>*)

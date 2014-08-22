(** Type elaboration from implicitely typed ML to explicitely typed ML. *)

open Misc
open Positions
open Name
open IAST
open Types
open ConstraintGeneration
open ConstraintSolver
open ExternalizeTypes
open InferenceErrors

let string_of_type ty =
  ASTio.(XAST.(to_string pprint_ml_type ty))

let string_of_type_scheme (TyScheme (ts, _, ty)) =
  String.concat " " (List.map (fun (TName x) -> x) ts)
  ^ "." ^ string_of_type ty

let elaborate : ConstraintSolver.answer -> IAST.program -> XAST.program =
  fun e p ->
    let datacon_schemes = ref [] in
    let save_constructor_type_scheme k s =
      datacon_schemes := (k, s) :: !datacon_schemes
    in
    let type_scheme_of_constructor k =
      try List.assoc k !datacon_schemes
      with Not_found -> assert false (** Because the program is well-typed. *)
    in

    let rec program p = List.map block p

    and block = function
      | BClassDefinition c ->
        XAST.BClassDefinition (class_definition c)
      | BTypeDefinitions ts ->
        XAST.BTypeDefinitions (type_mutual_definitions ts)
      | BDefinition d ->
        XAST.BDefinition (bind_value d)
      | BInstanceDefinitions is ->
        XAST.BInstanceDefinitions (List.map instance_definition is)

    and class_definition cd =
      {
        XAST.class_name      = cd.class_name;
        XAST.class_position  = cd.class_position;
        XAST.class_members   = cd.class_members;
        XAST.superclasses    = cd.superclasses;
        XAST.class_parameter = cd.class_parameter
      }

    and instance_definition id =
      {
        XAST.instance_position = id.instance_position;
        XAST.instance_parameters = id.instance_parameters;
        XAST.instance_class_name = id.instance_class_name;
        XAST.instance_typing_context = id.instance_typing_context;
        XAST.instance_index = id.instance_index;
        XAST.instance_members = List.map record_binding id.instance_members;
      }

    and type_mutual_definitions (TypeDefs (pos, tds)) =
      XAST.TypeDefs (pos, List.map type_definition tds)

    and type_definition = function
      | TypeDef (pos, k, tname, adt) ->
        XAST.TypeDef (pos, k, tname, datatype_definition adt)

      | ExternalType (pos, ts, t, os) ->
        XAST.ExternalType (pos, ts, t, os)

    and datatype_definition = function
      | DRecordType (ts, ltys) ->
        XAST.DRecordType (ts, ltys)

      | DAlgebraic ds ->
        let ds = List.map datacon ds in
        let ts = (fun (_, _, ts, _) -> ts) (List.hd ds) in
        let ds = List.map (fun (p, d, ts', ty) ->
          let tvs = List.map (fun v -> (TyVar (p, v))) ts in
          (p, d, ts, Types.substitute (List.combine ts' tvs) ty)
        ) ds
        in
        XAST.DAlgebraic ds

    and datacon (pos, (DName x as d), _, _) =
      let (TyScheme (_, _, ty)) = type_scheme_of pos x in
      let ts =
        match snd (destruct_ntyarrow ty) with
          | TyApp (_, _, tys) ->
            List.map (function
              | TyVar (_, v) ->
                v
              | _ ->
              (** Because of the syntax of datacon declarations. *)
                assert false
            ) tys
          | _ ->
          (** Because of the syntax of datacon declarations. *)
            assert false
      in
      save_constructor_type_scheme d (TyScheme (ts, [], ty));
      (pos, d, ts, ty)

      and instantiation (TyScheme (ts, _, sty)) ty =
        let rec aux subst sty ty =
          match sty, ty with
            | TyVar (_, x), ty ->
              (x, ty) :: subst
            | TyApp (_, _, ts), TyApp (_, _, tys) ->
              (** Type inference generates valid instantiations, so: *)
              assert (List.(length ts = length tys));
              List.fold_left2 aux subst ts tys
            | _, _ ->
              (** Type inference generates valid instantiations. *)
              assert false
        in
        let subst = aux [] sty ty in
        List.(map (fun x ->
          try
            assoc x subst
          with Not_found ->
            TyVar (undefined_position, x)
        ) ts)

      and instantiation_of_identifier p s x =
        let instantiation_target =
          try
            type_of_variable p (ConstraintSolver.lookup_instantiation e (x, p))
          with Not_found ->
            Printf.eprintf "ICE: Cannot find an instantiation for %s\n" x;
            exit 1
        in
        instantiation s instantiation_target

      and expression = function
        | EVar (p, ((Name x) as n), _) ->
          let (TyScheme (ts, _, _)) as sx = type_scheme_of p x in
          let i = instantiation_of_identifier p sx x in
          XAST.EVar (p, n, i)

        | ELambda (p, b, t) ->
          XAST.ELambda (p, binding p e b, expression t)

        | EApp (p, a, b) ->
          XAST.EApp (p, expression a, expression b)

        | EBinding (p, bvs, t) ->
          XAST.EBinding (p, bind_value bvs, expression t)

        | EDCon (p, ((DName x) as k), _, ts) ->
          let sx = type_scheme_of_constructor k in
          XAST.EDCon (p, k,
                      instantiation_of_identifier p sx x,
                      List.map expression ts)

        | EPrimitive (p, prim) ->
          XAST.EPrimitive (p, primitive prim)

        | ERecordCon (p, ((Name x) as n), _, rbs) ->
          let (_, _, v) = lookup_binding e x in
          let i =
            match type_of_variable p v with
              | TyApp (_, _, tys) ->
                tys
              | _ ->
                (** Because inference only produces well-formed type. *)
                assert false
          in
          XAST.ERecordCon (p, n, i, List.map record_binding rbs)

        | ERecordAccess (p, e, l) ->
          XAST.ERecordAccess (p, expression e, l)

        | EMatch (p, s, bs) ->
          XAST.EMatch (p, expression s, List.map branch bs)

        | EForall (_, _, e) | EExists (_, _, e) | ETypeConstraint (_, e, _) ->
          (** We erase the type annotations provided by the user because
              type inference took different names for type variables. *)
          expression e

      and branch (Branch (p, pat, e)) =
        XAST.Branch (p, pattern pat, expression e)

      and pattern = function
        | PVar (p, x) ->
          XAST.PVar (p, x)

        | PWildcard p ->
          XAST.PWildcard p

        | PAlias (p, n, pat) ->
          XAST.PAlias (p, n, pattern pat)

        | PTypeConstraint (p, pat, ty) ->
          XAST.PTypeConstraint (p, pattern pat, ty)

        | PPrimitive (p, prim) ->
          XAST.PPrimitive (p, primitive prim)

        | PData (p, (DName x as k), _, ps) ->
          let sx = type_scheme_of_constructor k in
          XAST.PData (p, k,
                      instantiation_of_identifier p sx x,
                      List.map pattern ps)

        | PAnd (p, ps) ->
          XAST.PAnd (p, List.map pattern ps)

        | POr (p, ps) ->
          XAST.POr (p, List.map pattern ps)

      and record_binding (RecordBinding (l, e)) =
        XAST.RecordBinding (l, expression e)

      and primitive = function
        | PIntegerConstant x -> XAST.PIntegerConstant x
        | PCharConstant c -> XAST.PCharConstant c
        | PUnit -> XAST.PUnit

      and bind_value = function
        | BindValue (p, vs) ->
          XAST.BindValue (p, List.map value_definition vs)

        | BindRecValue (p, vs) ->
          XAST.BindRecValue (p, List.map value_definition vs)

        | ExternalValue (p, ts, (x, ty), os) ->
          let ty = match ty with
            | None -> assert false (** By syntax. *)
            | Some ty -> ty
          in
          XAST.ExternalValue (p, ts, (x, ty), os)

      and type_scheme_of pos x =
        let b = lookup_binding e x in
        type_scheme_of_variable pos b

      and value_definition (ValueDef (pos, _, c, b, t)) =
        let (x, (TyScheme (ts, c, ty) as s)) =
          match destruct_binding b with
            | ((Name x) as n, _) -> (n, type_scheme_of pos x)
        in
        let eforall = function
          | [] -> expression
          | ts -> fun t -> XAST.EForall (pos, ts, expression t)
        in
        XAST.ValueDef (pos, ts, c, (x, ty), eforall ts t)

      and binding pos e b =
      (** We erase the type annotations provided by the user
          because type inference took different names for
          rigid type variables. *)
        match destruct_binding b with
          | ((Name x) as n, _) ->
            let (_, _, v) = lookup_binding e x in
            (n, type_of_variable pos v)
      in
      program p

let program : IAST.program -> XAST.program = fun p ->
  let print_variable pos x =
    let ty = ExternalizeTypes.type_of_variable pos x in
    ASTio.(to_string IAST.pprint_ml_type ty)
  in
  InferenceErrors.handle_error print_variable (fun () ->
    let p = AlphaRename.program p in
    let c = generate_constraint p in
    let e = solve c in
    let x = elaborate e p in
    x
  )

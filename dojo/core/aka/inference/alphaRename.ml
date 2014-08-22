(** Alpha-renaming of binders. *)

open Name
open IAST

exception UnboundVariable of Positions.position * name
exception OverloadedSymbolCannotBeBound of Positions.position * name

let program p =
  let exclusive_name = -1 in
  let fresh_name =
    let t = Hashtbl.create 13 in
    fun pos ?(exclusive=false) ((Name n) as x) ->
      let occ =
        try
          let occ = Hashtbl.find t n in
          if exclusive || occ = exclusive_name then
            raise (OverloadedSymbolCannotBeBound (pos, x))
          else
            succ occ
        with Not_found ->
          if exclusive then exclusive_name else 1
      in
      Hashtbl.replace t n occ;
      if occ = 1 then
        Name n
      else
        Name (n ^ "_" ^ string_of_int (pred occ))
  in
  let lookup pos n s =
    try
      List.assoc n s
    with Not_found -> raise (UnboundVariable (pos, n))
  in
  let fresh_binder pos n s =
    let n' = fresh_name pos n in
    (n', (n, n') :: s)
  in
  let rec program p =
    fst (Misc.list_foldmap block [] p)

  and block s = function
    | BClassDefinition c ->
      (BClassDefinition c, class_definition s c)
    | BInstanceDefinitions is ->
      (BInstanceDefinitions (List.map (instance_definition s) is), s)
    | BTypeDefinitions ts ->
      (BTypeDefinitions ts, s)
    | BDefinition d ->
      let d, s = value_binding s d in
      (BDefinition d, s)

  and class_definition s ct =
    List.fold_left (fun ms (pos, LName x, _) ->
      ignore (fresh_name pos ~exclusive:true (Name x));
      (Name x, Name x) :: ms
    ) s ct.class_members

  and instance_definition s ti = List.(
    { ti with instance_members = map (record_binding s) ti.instance_members }
  )

  and expression s = function
    | EVar (pos, n, i) ->
      EVar (pos, lookup pos n s, i)

    | ELambda (pos, b, e) ->
      let (b, s) = binding pos s b in
      ELambda (pos, b, expression s e)

    | EApp (pos, a, b) ->
      let a = expression s a in
      let b = expression s b in
      EApp (pos, a, b)

    | EBinding (pos, vb, e) ->
      let (vb, s) = value_binding s vb in
      EBinding (pos, vb, expression s e)

    | EPrimitive (pos, prim) ->
      EPrimitive (pos, prim)

    | EForall (pos, ts, e) ->
      EForall (pos, ts, expression s e)

    | EExists (pos, ts, e) ->
      EExists (pos, ts, expression s e)

    | ETypeConstraint (pos, e, ty) ->
      ETypeConstraint (pos, expression s e, ty)

    | EDCon (pos, d, i, es) ->
      EDCon (pos, d, i, List.map (expression s) es)

    | EMatch (pos, e, bs) ->
      EMatch (pos, expression s e, List.map (branch s) bs)

    | ERecordAccess (pos, e, l) ->
      ERecordAccess (pos, expression s e, l)

    | ERecordCon (pos, z, i, rbs) ->
      (** We do not rename [z] because they already are unique and fresh. *)
      ERecordCon (pos, z, i, List.map (record_binding s) rbs)

  and value_binding s = function
    | BindValue (pos, vs) ->
      let (vs, s) = Misc.list_foldmap value_definition s vs in
      (BindValue (pos, vs), s)

    | BindRecValue (pos, vs) -> List.(
      let bs = map (fun (ValueDef (_, _, _, b, _)) -> b) vs in
      let (bs, s) = Misc.list_foldmap (binding pos) s bs in
      let vs =
        map2
          (fun (ValueDef (p, ts, cs, _, e)) b ->
            ValueDef (p, ts, cs, b, expression s e))
          vs bs
      in
      (BindRecValue (pos, vs), s)
    )

    | ExternalValue (pos, ts, b, os) ->
      let (b, s) = binding pos s b in
      (ExternalValue (pos, ts, b, os), s)

  (** Pattern matching clause. *)
  and branch s (Branch (pos, p, e)) =
    let (p, s) = pattern s p in
    Branch (pos, p, expression s e)

  and record_binding s (RecordBinding (l, e)) =
    RecordBinding (l, expression s e)

  (** A value definition consists of a list of explicit universal
      quantifiers, a pattern, and an expression. *)
  and value_definition s (ValueDef (pos, ts, c, b, e)) =
    let (b, s) = binding pos s b in
    (ValueDef (pos, ts, c, b, expression s e), s)

  and binding pos s (x, ty) =
    let (y, s) = fresh_binder pos x s in
    ((y, ty), s)

  and pattern s = function
    | PVar (pos, n) ->
      let (y, s) = fresh_binder pos n s in
      (PVar (pos, y), s)

    | PWildcard pos ->
      (PWildcard pos, s)

    | PAlias (pos, n, p) ->
      let (y, s) = fresh_binder pos n s in
      let (p, s) = pattern s p in
      (PAlias (pos, y, p), s)

    | PTypeConstraint (pos, p, ty) ->
      let (p, s) = pattern s p in
      (PTypeConstraint (pos, p, ty), s)

    | PPrimitive (pos, p) ->
      (PPrimitive (pos, p), s)

    | PData (pos, d, i, ps) ->
      let (ps, s) = Misc.list_foldmap pattern s ps in
      (PData (pos, d, i, ps), s)

    | PAnd (pos, ps) ->
      let (ps, s) = Misc.list_foldmap pattern s ps in
      (PAnd (pos, ps), s)

    | POr (pos, ps) ->
      let (ps, s) = Misc.list_foldmap pattern s ps in
      (POr (pos, ps), s)
  in
  program p

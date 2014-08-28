open ExtPervasives
open XAST
open Name

type value =
  | VClosure of env * name * expression
  | VData    of dname * value list
  | VRecord  of (lname * value) list
  | VPrimitive of primitive

and env = (name * value ref) list

exception UnboundIdentifier of string
exception UnboundLabel of string

let lookup ((Name n) as x) env = try ! (List.assoc x env)
  with Not_found -> raise (UnboundIdentifier n)

let bind x v env = (x, ref v) :: env

let update ((Name n) as x) v (env : env) = try ((List.assoc x env) := v)
  with Not_found -> raise (UnboundIdentifier n)

let empty = []

let rec program env bs = List.fold_left block env bs

and block env = function
  | BDefinition vb -> value_binding env vb
  | _ -> env

and value_binding env = function
  | BindValue (_, vdefs) ->
    let env, _ = list_foldmap value_definition env vdefs in
    env

  | BindRecValue (_, vdefs) ->
    let env, vs = list_foldmap value_definition env vdefs in
    List.iter (fun (x, v) ->
      match v with
        | VClosure (_, y, e) -> update x (VClosure (env, y, e)) env
        | _ -> assert false (* We only have recursive immediate functions. *)
    ) vs;
    env

  | ExternalValue _ ->
    failwith "TODO"

and value_definition env (ValueDef (_, _, _, (x, _), t)) =
  let v = expression env t in
  (bind x v env, (x, v))


and expression env = function
    (** Core ML. *)
    | EVar (_, x, _) -> lookup x env
    | ELambda (_, (x, _), t) -> VClosure (env, x, t)
    | EApp (_, a, b) -> begin match expression env a with
        | VClosure (env', x, e) ->
          expression (bind x (expression env b) env') e
        | _ -> assert false (* By typing. *)
    end

    | EBinding (_, b, t) ->
      expression (value_binding env b) t

    | EPrimitive (_, p) ->
      VPrimitive p

    (** Type abstraction. *)
    | EForall (_, _, t) | EExists (_, _, t) | ETypeConstraint (_, t, _) ->
      expression env t

    (** Algebraic datatypes. *)
    | EDCon (_, k, _, es) ->
      VData (k, List.map (expression env) es)

    | EMatch (_, e, bs) ->
      branches env (expression env e) bs

    (** Records. *)
    | ERecordAccess (_, e, ((LName n) as l)) ->
      begin match expression env e with
        | VRecord vs -> begin try List.assoc l vs
          with Not_found -> raise (UnboundLabel n)
        end
        | _ -> assert false (* By typing. *)
      end

    | ERecordCon (_, _, _, rs) ->
      VRecord (List.map (record_binding env) rs)

(** Pattern matching branch. *)
and branches env sv = function
  | [] ->
    failwith "Match failure" (* FIXME *)
  | Branch (_, p, e) :: bs ->
    begin match pattern (Some env) sv p with
      | Some env -> expression env e
      | None -> branches env sv bs
    end

and record_binding env (RecordBinding (l, e)) =
  (l, expression env e)

and pattern (env : env option) sv p =
  match env with
    | None -> None
    | Some env ->
      match p, sv with
        | PVar (_, x), v -> Some (bind x v env)
        | PWildcard _, _ -> Some env
        | PAlias (_, x, p), v -> pattern (Some (bind x sv env)) sv p
        | PTypeConstraint (_, p, _), _ -> pattern (Some env) sv p
        | PPrimitive (_, p'), VPrimitive p when p = p' -> Some env
        | PData (_, k, _, ps), VData (k', vs) when k = k' ->
          begin
            assert (List.(length ps = length vs)); (* By typing. *)
            List.fold_left2 pattern (Some env) vs ps
          end
        | PAnd (_, ps), _ ->
          List.fold_left (function
            | Some env -> fun p -> pattern (Some env) sv p
            | None -> fun p -> None
          ) (Some env) ps
        | POr (_, ps), _ ->
          first_match env sv ps
        | _, _ ->
          None

and first_match env sv = function
  | [] -> None
  | p :: ps ->
    begin match pattern (Some env) sv p with
      | None -> first_match env sv ps
      | Some env -> Some env
    end

let program t = Lwt.return (program empty t)

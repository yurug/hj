open ExtPervasives
open XAST
open Name
open Lwt

type value =
  | VClosure of env * name * expression
  | VPrimitiveFun of (value -> value Lwt.t)
  | VData    of dname * value list
  | VRecord  of (lname * value) list
  | VPrimitive of primitive

and env = (name * value ref) list

let lookup_all_matching regexp env =
  List.filter (fun (Name x, v) -> Str.string_match regexp x 0) env

exception UnboundIdentifier of string
exception UnboundLabel of string

(** To be set by {!User}. *)
let user_has_tag = ref (fun _ _ -> assert false)

(* FIXME: Use GADT to lift the following functions
   FIXME: in a cleaner way. *)

let lookup_primitive = function
  | "string_append" -> return (VPrimitiveFun (function
      | VPrimitive (PStringConstant s) ->
        return (VPrimitiveFun (function
          | VPrimitive (PStringConstant s') ->
            return (VPrimitive (PStringConstant (s ^ s')))
          | _ -> assert false (* By typing .*)
        ))
      | _ -> assert false (* By typing .*)
  ))

  | "user_has_tag" -> return (VPrimitiveFun (function
      | VPrimitive (PStringConstant s) ->
        return (VPrimitiveFun (function
          | VPrimitive (PStringConstant t) ->
            !user_has_tag s t >>= (function
              | true -> return (VData (DName "True", []))
              | false -> return (VData (DName "False", []))
            )
          | _ -> assert false (* By typing. *)
        ))
      | _ -> assert false (* By typing. *)
  ))
  | _ -> raise_lwt Not_found

let lookup ((Name n) as x) env =
  try return (!(List.assoc x env))
  with Not_found ->
    try lookup_primitive n
    with Not_found ->
      raise (UnboundIdentifier n)

let bind x v env = (x, ref v) :: env

let update ((Name n) as x) v (env : env) = try ((List.assoc x env) := v)
  with Not_found -> raise (UnboundIdentifier n)

let empty = []

let rec program env bs = Lwt_list.fold_left_s block env bs

and block env = function
  | BDefinition vb -> value_binding env vb
  | _ -> return env

and value_binding env = function
  | BindValue (_, vdefs) ->
    lwt env, _ = lwt_list_foldmap value_definition env vdefs in
    return env

  | BindRecValue (_, vdefs) ->
    lwt env, vs = lwt_list_foldmap value_definition env vdefs in
    List.iter (fun (x, v) ->
      match v with
        | VClosure (_, y, e) -> update x (VClosure (env, y, e)) env
        | _ -> assert false (* We only have recursive immediate functions. *)
    ) vs;
    return env

  | ExternalValue (_, _, (x, _), p) ->
    lwt p = lookup_primitive p in
    return (bind x p env)

and value_definition env (ValueDef (_, _, _, (x, _), t)) =
  lwt v = expression env t in
  return (bind x v env, (x, v))


and expression env = function
    (** Core ML. *)
    | EVar (_, x, _) ->
      lookup x env
    | ELambda (_, (x, _), t) ->
      return (VClosure (env, x, t))
    | EApp (_, a, b) -> begin expression env a >>= function
        | VClosure (env', x, e) ->
          lwt v = expression env b in
          expression (bind x v env') e
        | VPrimitiveFun f ->
          expression env b >>= f
        | _ -> assert false (* By typing. *)
    end

    | EBinding (_, b, t) ->
      lwt env = value_binding env b in
      expression env t

    | EPrimitive (_, p) ->
      return (VPrimitive p)

    (** Type abstraction. *)
    | EForall (_, _, t) | EExists (_, _, t) | ETypeConstraint (_, t, _) ->
      expression env t

    (** Algebraic datatypes. *)
    | EDCon (_, k, _, es) ->
      lwt vs = Lwt_list.map_s (expression env) es in
      return (VData (k, vs))

    | EMatch (_, e, bs) ->
      lwt v = expression env e in
      branches env v bs

    (** Records. *)
    | ERecordAccess (_, e, ((LName n) as l)) ->
      begin expression env e >>= function
        | VRecord vs -> begin try_lwt return (List.assoc l vs)
          with Not_found -> raise_lwt (UnboundLabel n)
        end
        | _ -> assert false (* By typing. *)
      end

    | ERecordCon (_, _, _, rs) ->
      lwt fs = Lwt_list.map_s (record_binding env) rs in
      return (VRecord fs)

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
  lwt v = expression env e in
  return (l, v)

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

let program t = program empty t

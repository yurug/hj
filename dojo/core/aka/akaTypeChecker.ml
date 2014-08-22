open ExtPervasives
open AkaAST

module TypeEnv = struct

  let initial = []

end

exception InferTypeArgument of AkaCST.term'
exception TypeClash         of AkaCST.term' * ty * ty option

let rec check_equal_type ty1 ty2 =
  assert false

let elaborate =

  let rec t ds = list_foldmap declaration TypeEnv.initial ds

  and declaration = function
    | ValueDecl (l, a, t) ->
      let ty = may_check env t a in
      (TypeEnv.bind l ty env, ValueDecl (l, option_ty a, term' t))

  and infer_term env cst = function
    | Lit l ->
      let ty = infer_literal env l in
      (ty, Lit l)

    | Variable n ->
      (TypeEnv.lookup env n, Variable n)

    | Lam (x, a, t) -> begin
      match a with
        | None ->
          raise (CannotInferTypeArgument cst)
        | Some ity ->
          let env = TypeEnv.bind x ity env in
          let oty, t = infer_term' env t in
          (mk_arrow ity oty, Lam (x, Some ity, t))
    end

    | App (a, b) ->
      let ty, a = infer_term' env a in begin
        match destruct_arrow ty with
          | Some (ity, oty) ->
            let b = check_term' env ity b in
            (oty, App (a, b))
          | None ->
            raise (CannotInferTypeOfFunction a)
      end

  and infer_term' env t = infer_term env (fst t) (snd t)

  and infer_literal = function
    | C.LUnit -> TUnit
    | C.LInt x -> TInt
    | C.LFloat f -> TFloat
    | C.LString s -> TString

  and check_term env cst ty = function
    | Lam (x, a, t) -> begin
      match destruct_arrow ty with
        | Some (ity, oty) ->
          check_annotation a ity;
          let env = TypeEnv.bind x ity env in
          let t = check_term' env oty t in
          Lam (x, Some ity, t)
        | None ->
          raise (InvalidType (cst, ty, None))
    end

    | t ->
      let ity = infer_term env cst t in
      if not (equal_type ty ity) then raise (TypeClash (cst, ty, Some ity))

  in
  t

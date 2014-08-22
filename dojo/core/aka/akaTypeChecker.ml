open ExtPervasives
open AkaAST

(** This module implements a very basic birectional type-checker
   for Aka. *)

(* FIXME: This should be replaced by a full type inference engine
   in the future. *)

module TypeEnv = struct

  type t = (variable * ty) list deriving (Json)

  let initial = []

  let lookup env k = List.assoc k env

  let bind env k v = (k, v) :: env

end

exception CannotInfer of AkaCST.term'
exception IllTyped    of AkaCST.term' * ty
exception TypeClash   of AkaCST.term' * ty * ty

let unit_ty = TApp (TVariable "unit", [])

let int_ty = TApp (TVariable "int", [])

let string_ty = TApp (TVariable "string", [])

let float_ty = TApp (TVariable "float", [])

let arrow_symbol = TVariable "->"

let mk_arrow ty1 ty2 =
  TApp (arrow_symbol, [ty1; ty2])

let destruct_arrow = function
  | TApp (k, [ity; oty]) when k = arrow_symbol -> Some (ity, oty)
  | _ -> None

let rec equal_type (TApp (k1, ts1)) (TApp (k2, ts2)) =
  List.(length ts1 = length ts2 && k1 = k2 && for_all2 equal_type ts1 ts2)

let elaborate p =

  let rec t ds = list_foldmap declaration TypeEnv.initial ds

  and declaration env = function
    | ValueDecl (l, a, t) ->
      let (ty, t) = may_check env t a in
      (TypeEnv.bind env l ty, ValueDecl (l, Some ty, t))

  and may_check env t = function
    | None -> infer_term' env t
    | Some ty -> (ty, check_term' env ty t)

  and infer_term env cst = function
    | Lit l ->
      let ty = infer_literal env l in
      (ty, Lit l)

    | Variable n ->
      (TypeEnv.lookup env n, Variable n)

    | Lam (x, a, t) -> begin
      match a with
        | None ->
          raise (CannotInfer cst)
        | Some ity ->
          let env = TypeEnv.bind env x ity in
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
            raise (CannotInfer (fst a))
      end

  and infer_term' env t =
    let (ty, t') = infer_term env (fst t) (snd t) in
    (ty, (fst t, t'))

  and infer_literal env = function
    | LUnit -> unit_ty
    | LInt x -> int_ty
    | LFloat f -> float_ty
    | LString s -> string_ty

  and check_term env cst ty = function
    | Lam (x, a, t) -> begin
      match destruct_arrow ty with
        | Some (ity, oty) ->
          check_annotation cst ity a;
          let env = TypeEnv.bind env x ity in
          let t = check_term' env oty t in
          Lam (x, Some ity, t)
        | None ->
          raise (IllTyped (cst, ty))
    end

    | t ->
      let ity, t = infer_term env cst t in
      if not (equal_type ty ity) then raise (TypeClash (cst, ty, ity));
      t

  and check_term' env ty t =
    let t' = check_term env (fst t) ty (snd t) in
    (fst t, t')

  and check_annotation cst ity = function
    | Some a when not (equal_type a ity) -> raise (TypeClash (cst, a, ity))
    | _ -> ()
  in
  t p

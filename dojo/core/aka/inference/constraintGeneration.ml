(**************************************************************************)
(*  Adaptated from:                                                       *)
(*  Mini, a type inference engine based on constraint solving.            *)
(*  Copyright (C) 2006. François Pottier, Yann Régis-Gianas               *)
(*  and Didier Rémy.                                                      *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation; version 2 of the License.               *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful, but   *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  General Public License for more details.                              *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program; if not, write to the Free Software           *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *)
(*  02110-1301 USA                                                        *)
(*                                                                        *)
(**************************************************************************)

(** This module implements typing constraint generation. *)

open Positions
open Misc
open KindInferencer
open Constraint
open InferenceTypes
open TypeAlgebra
open MultiEquation
open TypingEnvironment
open InferenceExceptions
open Types
open InternalizeTypes
open Name
open IAST

(** {2 Inference} *)

let ctx0 = fun c -> c

let ( @@ ) ctx1 ctx2 = fun c -> ctx1 (ctx2 c)

let fold env f =
  List.fold_left (fun (env, ctx) x ->
    let (env, ctx') = f env x in
    (env, ctx @@ ctx')
  ) (env, ctx0)

(** A fragment denotes the typing information acquired in a match branch.
    [gamma] is the typing environment coming from the binding of pattern
    variables. [vars] are the fresh variables introduced to type the
    pattern. [tconstraint] is the constraint coming from the instantiation
    of the data constructor scheme. *)
type fragment =
    {
      gamma       : (crterm * position) StringMap.t;
      vars        : variable list;
      tconstraint : tconstraint;
    }

(** The [empty_fragment] is used when nothing has been bound. *)
let empty_fragment =
  {
    gamma       = StringMap.empty;
    vars        = [];
    tconstraint = CTrue undefined_position;
  }

(** Joining two fragments is straightforward except that the environments
    must be disjoint (a pattern cannot bound a variable several times). *)
let rec join_fragment pos f1 f2 =
  {
    gamma =
      (try
        StringMap.strict_union f1.gamma f2.gamma
      with StringMap.Strict x -> raise (NonLinearPattern (pos, Name x)));
    vars        = f1.vars @ f2.vars;
    tconstraint = f1.tconstraint ^ f2.tconstraint;
  }

(** [infer_pat_fragment p t] generates a fragment that represents the
    information gained by a success when matching p. *)
and infer_pat_fragment tenv p t =
  let join pos = List.fold_left (join_fragment pos) empty_fragment in
  let rec infpat t = function

    (** Wildcard pattern does not generate any fragment. *)
    | PWildcard pos ->
        empty_fragment

    (** We refer to the algebra to know the type of a primitive. *)
    | PPrimitive (pos, p) ->
        { empty_fragment with
            tconstraint = (t =?= type_of_primitive (as_fun tenv) p) pos
        }

    (** Matching against a variable generates a fresh flexible variable,
        binds it to the [name] and forces the variable to be equal to [t]. *)
    | PVar (pos, Name name) ->
        let v = variable Flexible () in
          {
            gamma       = StringMap.singleton name (TVariable v, pos);
            tconstraint = (TVariable v =?= t) pos;
            vars        = [ v ]
          }

    (** A disjunction forces the bounded variables of the subpatterns to
        be equal. For that purpose, we extract the types of the subpatterns'
        environments and we make them equal. *)
    | POr (pos, ps) ->
        let fps = List.map (infpat t) ps in
          (try
            let rgamma = (List.hd fps).gamma in
            let cs =
              List.fold_left (fun env_eqc fragment ->
                        StringMap.mapi
                          (fun k (t', _) ->
                             let (t, c) = StringMap.find k env_eqc in
                               (t, (t =?= t') pos ^ c))
                          fragment.gamma)
                (StringMap.mapi (fun k (t, _) -> (t, CTrue pos)) rgamma)
              fps
            in
            let c = StringMap.fold (fun k (_, c) acu -> c ^ acu) cs (CTrue pos)
            in
              {
                gamma       = rgamma;
                tconstraint = c ^ conj (List.map (fun f -> f.tconstraint) fps);
                vars        = List.flatten (List.map (fun f -> f.vars) fps)
              }
          with Not_found ->
            raise (InvalidDisjunctionPattern pos))

    (** A conjunction pattern does join its subpatterns' fragments. *)
    | PAnd (pos, ps) ->
        join pos (List.map (infpat t) ps)

    (** [PAlias (x, p)] is equivalent to [PAnd (PVar x, p)]. *)
    | PAlias (pos, Name name, p) ->
        let fragment = infpat t p in
          { fragment with
              gamma       = StringMap.strict_add name (t, pos) fragment.gamma;
              tconstraint = (SName name <? t) pos ^ fragment.tconstraint
          }

    (** A type constraint is taken into account by the insertion of a type
        equality between [t] and the annotation. *)
    | PTypeConstraint (pos, p, typ) ->
        let fragment = infpat t p
        and ityp = InternalizeTypes.intern pos tenv typ in
          { fragment with
              tconstraint = (ityp =?= t) pos ^ fragment.tconstraint
          }

    (** Matching against a data constructor generates the fragment that:
        - forces [t] to be the type of the constructed value ;
        - constraints the types of the subpatterns to be equal to the arguments
        of the data constructor. *)
    | PData (pos, (DName x as k), _, ps) ->
      let (alphas, kt) = fresh_datacon_scheme pos tenv k in
      let rt = result_type (as_fun tenv) kt
      and ats = arg_types (as_fun tenv) kt in
      if (List.length ps <> List.length ats) then
        raise (NotEnoughPatternArgts pos)
      else
        let fragment = join pos (List.map2 infpat ats ps) in
        let cinst = (SName x <? kt) pos in
        { fragment with
          tconstraint = cinst ^ fragment.tconstraint ^ (t =?= rt) pos ;
          vars        = alphas @ fragment.vars;
        }
  in
    infpat t p

(** Constraint contexts. *)
type context =
    (crterm, variable) type_constraint -> (crterm, variable) type_constraint

let header_of_binding pos tenv (Name x, ty) t =
  (match ty with
     | None -> CTrue pos
     | Some ty -> (intern pos tenv ty =?= t) pos),
  StringMap.add x (t, pos) StringMap.empty

let fresh_record_name =
  let r = ref 0 in
  fun () -> incr r; Name (Printf.sprintf "_record_%d" !r)

(** [intern_data_constructor adt_name env_info dcon_info] returns
    env_info augmented with the data constructor's typing information
    It also checks if its definition is legal. *)
let intern_data_constructor pos (TName adt_name) env_info dcon_info =
  let (tenv, acu, lrqs, let_env) = env_info
  and (pos, DName dname, qs, typ) = dcon_info in
  let rqs, rtenv = fresh_unnamed_rigid_vars pos tenv qs in
  let tenv' = add_type_variables rtenv tenv in
  let ityp = InternalizeTypes.intern pos tenv' typ in
  let _ =
    if not (is_regular_datacon_scheme tenv rqs ityp) then
      raise (InvalidDataConstructorDefinition (pos, DName dname))
  in
  let v = variable ~structure:ityp Flexible () in
    ((add_data_constructor tenv (DName dname)
        (InternalizeTypes.arity typ, rqs, ityp)),
     (DName dname, v) :: acu,
     (rqs @ lrqs),
     StringMap.add dname (ityp, pos) let_env)

let infer_typedef tenv (TypeDefs (pos, tds)) =
  let bind_new_tycon pos name tenv kind =
    (* Insert the type constructor into the environment. *)
    let ikind = KindInferencer.intern_kind (as_kind_env tenv) kind
    and ids_def = ref Abstract
    and ivar = variable ~name:name Constant () in
    let c = fun c' ->
      CLet ([Scheme (pos, [ivar], [], [], c', StringMap.empty)],
            CTrue pos)
    in
    (ids_def, add_type_constructor tenv name (ikind, ivar, ids_def), c)
  in

  List.fold_left
    (fun (tenv, c) -> function
      | TypeDef (pos', kind, name, DRecordType (ts, rts)) ->
        let ids_def, tenv, c = bind_new_tycon pos' name tenv kind in
        let rqs, rtenv = fresh_unnamed_rigid_vars pos tenv ts in
        let tenv' = add_type_variables rtenv tenv in
        let tyvs = List.map (fun v -> TyVar (pos', v)) ts in
        let rty =
          InternalizeTypes.intern pos' tenv' (TyApp (pos', name, tyvs))
        in
        let intern_label_type (pos, l, ty) =
          (l, InternalizeTypes.intern pos' tenv' ty)
        in
        ids_def := Product (rqs, rty, List.map intern_label_type rts);
        (tenv, c)

      | TypeDef (pos', kind, name, DAlgebraic ds) ->
        let ids_def, tenv, c = bind_new_tycon pos' name tenv kind in
        let (tenv, ids, rqs, let_env) =
          List.fold_left
            (intern_data_constructor pos name)
            (tenv, [], [], StringMap.empty)
            ds
        in
        ids_def := Sum ids;
        let c = fun c' ->
          c (CLet ([Scheme (pos', rqs, [], [],
                            CTrue pos',
                            let_env)],
                   c'))
        in
        (tenv, c)

      | ExternalType (pos, ts, name, _) ->
        let kind = kind_of_arity (List.length ts) in
        let ikind = KindInferencer.intern_kind (as_kind_env tenv) kind in
        let ivar = variable ~name Constant () in
        let tenv = add_type_constructor tenv name (ikind, ivar, ref Abstract) in
        (tenv,
         fun c ->
           CLet ([Scheme (pos, [ivar], [], [], c, StringMap.empty)], CTrue pos)
        )

    )
    (tenv, fun c -> c)
    tds

(** [infer_vdef pos tenv (pos, qs, p, e)] returns the constraint
    related to a value definition. *)
let rec infer_vdef pos tenv (ValueDef (pos, qs, cs, b, e)) =
  let rec is_value_form = function
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
  in
  if is_value_form e then
    let x = variable Flexible () in
    let tx = TVariable x in
    let rqs, rtenv = fresh_rigid_vars pos tenv qs in
    let tenv' = add_type_variables rtenv tenv in
    let xs, gs, cs = InternalizeTypes.intern_class_predicates pos tenv' cs in
    let c, h = header_of_binding pos tenv' b tx in
    ([], Scheme (pos, rqs, x :: xs,
            gs, c ^ conj cs ^ infer_expr tenv' e tx,
            h))
  else
    let x = variable Flexible () in
    let tx = TVariable x in
    let rqs, rtenv = fresh_rigid_vars pos tenv qs in
    let tenv' = add_type_variables rtenv tenv in
    let xs, gs, cs = InternalizeTypes.intern_class_predicates pos tenv' cs in
    let c, h = header_of_binding pos tenv' b tx in
    ([x],
     Scheme (pos, rqs, xs,
             gs, c ^ conj cs ^ infer_expr tenv' e tx,
             h))


(** [infer_binding tenv b] examines a binding [b], updates the
    typing environment if it binds new types or generates
    constraints if it binds values. *)
and infer_binding tenv b =
  match b with
    | ExternalValue (pos, ts, b, _) ->
      let x = variable Flexible () in
      let tx = TVariable x in
      let rqs, rtenv = fresh_rigid_vars pos tenv ts in
      let tenv' = add_type_variables rtenv tenv in
      let c, h = header_of_binding pos tenv' b tx in
      let scheme = Scheme (pos, rqs, [x], [], c, h) in
      tenv, (fun c -> CLet ([scheme], c))

    | BindValue (pos, vdefs) ->
      let xs, schemes = List.(split (map (infer_vdef pos tenv) vdefs)) in
      tenv, (fun c -> ex (List.flatten xs) (CLet (schemes, c)))

    | BindRecValue (pos, vdefs) ->

        (* The constraint context generated for
           [let rec forall X1 . x1 : T1 = e1
           and forall X2 . x2 = e2] is

           let forall X1 (x1 : T1) in
           let forall [X2] Z2 [
           let x2 : Z2 in [ e2 : Z2 ]
           ] ( x2 : Z2) in (
           forall X1.[ e1 : T1 ] ^
           [...]
           )

           In other words, we first assume that x1 has type scheme
           forall X1.T1.
           Then, we typecheck the recursive definition x2 = e2, making sure
           that the type variable X2 remains rigid, and generalize its type.
           This yields a type scheme for x2, which is then used to check
           that e1 actually has type scheme forall X1.T1.

           In the above example, there are only one explicitly typed and one
           implicitly typed value definitions.

           In the general case, there are multiple explicitly and implicitly
           typed definitions, but the principle remains the same. We generate
           a context of the form

           let schemes1 in

           let forall [rqs2] fqs2 [
           let h2 in c2
           ] h2 in (
           c1 ^
           [...]
           )

        *)

      let schemes1, rqs2, fqs2, cs2, h2, c2, c1 =
        List.fold_left
          (fun
            (schemes1, rqs2, fqs2, cs2, h2, c2, c1)
            (ValueDef (pos, qs, cs, b, e)) ->

              (* Allocate variables for the quantifiers in the list
                 [qs], augment the type environment accordingly. *)

              let rvs, rtenv = fresh_rigid_vars pos tenv qs in
              let tenv' = add_type_variables rtenv tenv in

              let (xs, gs, xcs) = intern_class_predicates pos tenv' cs in

              (* Check whether this is an explicitly or implicitly
                 typed definition. *)

              match InternalizeTypes.explicit_or_implicit pos b e with
                | InternalizeTypes.Implicit (Name name, e) ->

                  let v = variable Flexible () in
                  let t = TVariable v in

                  schemes1,
                  rvs @ rqs2,
                  v :: fqs2 @ xs,
                  gs @ cs2,
                  StringMap.add name (t, pos) h2,
                  conj xcs ^ infer_expr tenv' e t ^ c2,
                  c1

                | InternalizeTypes.Explicit (Name name, typ, e) ->

                  InternalizeTypes.intern_scheme pos tenv name qs cs typ
                  :: schemes1,
                  rqs2,
                  fqs2,
                  cs2,
                  h2,
                  c2,
                  fl rvs ~h:gs (ex xs (
                    conj xcs
                    ^ infer_expr tenv' e (InternalizeTypes.intern pos tenv' typ)
                  ))
                  ^ c1

                | _ -> assert false

          ) ([], [], [], [], StringMap.empty, CTrue pos, CTrue pos) vdefs in

      tenv,
      fun c -> CLet (schemes1,
                     CLet ([ Scheme (pos, rqs2, fqs2, cs2,
                                     CLet ([ monoscheme h2 ], c2), h2) ],
                           c1 ^ c)
      )

(** [infer_expr tenv d e t] generates a constraint that guarantees that [e]
    has type [t]. It implements the constraint generation rules for
    expressions. It may use [d] as an equation theory to prove coercion
    correctness. *)
and infer_expr tenv e (t : crterm) =
  match e with

    (** The [exists a. e] construction introduces [a] in the typing
        scope so as to be usable in annotations found in [e]. *)
    | EExists (pos, vs, e) ->
      let (fqs, denv) = fresh_flexible_vars pos tenv vs in
      let tenv = add_type_variables denv tenv in
      ex fqs (infer_expr tenv e t)

    | EForall (pos, vs, e) ->
      (** Not in the implicitly typed language. *)
      assert false

    (** The type of a variable must be at least as general as [t]. *)
    | EVar (pos, Name name, _) ->
      (SName name <? t) pos

    (** To type a lambda abstraction, [t] must be an arrow type.
        Furthermore, type variables introduced by the lambda pattern
        cannot be generalized locally. *)
    | ELambda (pos, b, e) ->
      exists (fun x1 ->
        exists (fun x2 ->
          let (c, h) = header_of_binding pos tenv b x1 in
          c
          ^ CLet ([ monoscheme h ], infer_expr tenv e x2)
          ^ (t =?= arrow tenv x1 x2) pos
        )
      )

    (** Application requires the left hand side to be an arrow and
        the right hand side to be compatible with the domain of this
        arrow. *)
    | EApp (pos, e1, e2) ->
      exists (fun x ->
        infer_expr tenv e1 (arrow tenv x t) ^ infer_expr tenv e2 x
      )

    (** A binding [b] defines a constraint context into which the
        constraint of [e] must be injected. *)
    | EBinding (_, b, e) ->
      snd (infer_binding tenv b) (infer_expr tenv e t)

    (** A type constraint inserts a type equality into the generated
        constraint. *)
    | ETypeConstraint (pos, e, typ) ->
      let ityp = intern pos tenv typ in
      (t =?= ityp) pos ^ infer_expr tenv e ityp

    (** The constraint of a [match] makes equal the type of the scrutinee
        and the type of every branch pattern. The body of each branch must
        be equal to [t]. *)
    | EMatch (pos, e, branches) ->
      exists (fun x ->
        infer_expr tenv e x ^
          conj
          (List.map
             (fun (Branch (pos, p, e)) ->
               let fragment = infer_pat_fragment tenv p x in
               CLet ([ Scheme (pos, [], fragment.vars, [],
                               fragment.tconstraint,
                               fragment.gamma) ],
                     infer_expr tenv e t))
             branches))

    (** A data constructor application is similar to usual application
        except that it must be fully applied. *)
    | EDCon (pos, (DName d as k), _, es) ->
      let arity, _, _ = lookup_datacon tenv k in
      let les = List.length es in
      if les <> arity then
        raise (PartialDataConstructorApplication (pos, arity, les))
      else
        exists_list es
          (fun xs ->
            let (kt, c) =
              List.fold_left (fun (kt, c) (e, x) ->
                arrow tenv x kt, c ^ infer_expr tenv e x)
                (t, CTrue pos)
                (List.rev xs)
            in
            c ^ (SName d <? kt) pos)

    (** We refers to the algebra to get the primitive's type. *)
    | EPrimitive (pos, c) ->
      (t =?= type_of_primitive (as_fun tenv) c) pos

    | ERecordCon (pos, Name k, _, []) ->
      let h = StringMap.add k (t, pos) StringMap.empty in
      CLet ([ monoscheme h ], (SName k <? t) pos)
      ^ infer_expr tenv (EPrimitive (pos, PUnit)) t

    (** The record definition by extension. *)
    | ERecordCon (pos, Name k, i, bindings) ->
      let ci =
        match i with
          | None -> CTrue pos
          | Some ty -> (intern pos tenv ty =?= t) pos
      in
      let h = StringMap.add k (t, pos) StringMap.empty in
      CLet ([ monoscheme h ], (SName k <? t) pos)
      ^ exists_list bindings
        (fun xs ->
          List.(
            let ls = map extract_label_from_binding bindings in
            let (vs, (rty, ltys)) = fresh_product_of_label pos tenv (hd ls) in
            ex vs (
              ci ^ (t =?= rty) pos
              ^ CConjunction (map (infer_label pos tenv ltys) xs)
            )
          )
        )

    (** Accessing the label [label] of [e1] requires [e1]'s type to
        be a record in which [label] is assign a [pre x] type. *)
    | ERecordAccess (pos, e1, label) ->
      exists (fun x ->
        exists (fun y ->
          let (vs, (rty, ltys)) = fresh_product_of_label pos tenv label in
          ex vs (
            infer_expr tenv e1 rty
            ^ (t =?= List.assoc label ltys) pos
          )
        )
      )

and extract_label_from_binding (RecordBinding (name, _)) =
  name

and infer_label pos tenv ltys (RecordBinding (l, exp), t) =
  try
    ((List.assoc l ltys) =?= t) pos ^ infer_expr tenv exp t
  with Not_found ->
    raise (IncompatibleLabel (pos, l))

(*<corrige>*)
let infer_class tenv tc =
  let pos = tc.class_position in

  (** Internalize the types of the class members as record types to
      check latter that the instances of this class have well-formed
      members. *)
  let class_ty = DRecordType ([tc.class_parameter], tc.class_members) in
  let class_kind = KArrow (KStar, KStar) in
  let class_tdef = TypeDef (pos, class_kind, tc.class_name, class_ty) in
  let tenv, ctx = infer_typedef tenv (TypeDefs (pos, [class_tdef])) in

  (** For each member [x] of type [T_x], we declare a toplevel value
      of type scheme:

      forall a. [K a] T_x
  *)
  let ctx_of_member ctx (pos, LName k, ty) =
    let (rqs, denv) = fresh_rigid_vars pos tenv [tc.class_parameter] in
    let tenv' = add_type_variables denv tenv in
    fun c ->
      let x = variable Flexible () in
      let tx = TVariable x in
      CLet ([Scheme (pos, rqs, [x], [],
                     (tx =?= intern pos tenv' ty) pos ^
                     CPredicate (pos, tc.class_name,
                                 TVariable (List.hd rqs)),
                     StringMap.singleton k (tx, pos))],
            ctx c)
  in
  let ctx_of_members = List.fold_left ctx_of_member ctx0 tc.class_members in

  try
    ConstraintSimplifier.add_implication tc.class_name tc.superclasses;
    (tenv, ctx_of_members)
  with
    | ConstraintSimplifier.MultipleClassDefinitions k ->
      raise (InferenceExceptions.MultipleClassDefinitions (pos, k))
    | ConstraintSimplifier.UnboundClass k ->
      raise (InferenceExceptions.UnboundClass (pos, k))
(*</corrige>*)

(*<sujet>
let infer_class tenv tc =
  (* Student! This is your job! *)
  (tenv, fun c -> c)
</sujet>*)

(*<corrige>*)
let infer_instance tenv ti =
  let pos = ti.instance_position in

  let register_instance_simplification_rule () =
    let (bs, denv) = fresh_flexible_vars pos tenv ti.instance_parameters in
    let tenv' = add_type_variables denv tenv in
    let k = ti.instance_class_name in
    (* FIXME: Do it more elegantly. *)
    let tvar_in_env v =
      match (as_fun tenv') v with TVariable v -> v | _ -> assert false
    in
    let g = tvar_in_env ti.instance_index in
    let kbs =
      List.map (fun (ClassPredicate (k, v)) ->
        (k, tvar_in_env v)
      ) ti.instance_typing_context
    in
    try
      ConstraintSimplifier.equivalent bs k g kbs
    with ConstraintSimplifier.OverlappingInstances (k, g) ->
      raise (InferenceExceptions.OverlappingInstances (pos, k, g))
  in
  let ctx c =
    let (rqs, denv) = fresh_rigid_vars pos tenv ti.instance_parameters in
    let typs = List.map (fun v -> TyVar (pos, v)) ti.instance_parameters in
    let tenv' = add_type_variables denv tenv in
    let ity = TyApp (pos, ti.instance_index, typs) in
    fl rqs (
      let ity = TyApp (pos, ti.instance_class_name, [ity]) in
      let ity = intern pos tenv' ity in
      let i = instantiation (start_of_position pos) LeftImplicit in
      let ms = ERecordCon (pos, fresh_record_name (), i, ti.instance_members) in
      infer_expr tenv' ms ity
    ) ^ c

  in
  register_instance_simplification_rule ();
  (tenv, ctx)
(*</corrige>*)

(*<sujet>
let infer_instance tenv ti =
  (* Student! This is your job! *)
  (tenv, fun c -> c)
</sujet>*)

(** [infer e] determines whether the expression [e] is well-typed
    in the empty environment. *)
let infer tenv e =
  exists (infer_expr tenv e)

(** [bind b] generates a constraint context that describes the
    top-level binding [b]. *)
let bind env b =
  infer_binding env b

let rec infer_program env p =
  let (env, ctx) = fold env block p in
  ctx (CDump undefined_position)

and block env = function
  | BClassDefinition ct -> infer_class env ct
  | BTypeDefinitions ts -> infer_typedef env ts
  | BInstanceDefinitions is -> fold env infer_instance is
  | BDefinition d -> infer_binding env d

let init_env () =
  let builtins =
    init_builtin_env (fun ?name () -> variable Rigid ?name:name ())
  in

  (* Add the builtin data constructors into the environment. *)
  let init_ds adt_name acu ds =
    let (env, acu, lrqs, let_env) as r =
      List.fold_left
        (fun acu (d, rqs, ty) ->
          intern_data_constructor undefined_position adt_name acu
            (undefined_position, d, rqs, ty)
        ) acu ds
    in
    (acu, r)
  in

  (* For each builtin algebraic datatype, define a type constructor
     and related data constructors into the environment. *)
  let (init_env, acu, lrqs, let_env) =
    List.fold_left
      (fun (env, dvs, lrqs, let_env) (n, (kind, v, ds)) ->
        let r = ref Abstract in
        let env = add_type_constructor env n
          (KindInferencer.intern_kind (as_kind_env env) kind,
           variable ~name:n Constant (),
           r)
        in
        let (dvs, acu) = init_ds n (env, dvs, lrqs, let_env) ds in
        r := Sum dvs;
        acu
      )
      (empty_environment, [], [], StringMap.empty)
      (List.rev builtins)
  in
  let vs =
    fold_type_info (fun vs (n, (_, v, _)) -> v :: vs) [] init_env
  in
  (* The initial environment is implemented as a constraint context. *)
  ((fun c ->
       CLet ([ Scheme (undefined_position, vs, [], [],
                       CLet ([ Scheme (undefined_position, lrqs, [], [],
                                       CTrue undefined_position,
                                       let_env) ],
                             c),
                       StringMap.empty) ],
             CTrue undefined_position)),
  vs, init_env)

let generate_constraint b =
  let (ctx, vs, env) = init_env () in
  (ctx (infer_program env b))

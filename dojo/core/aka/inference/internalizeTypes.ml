(**************************************************************************)
(* Adapted from:                                                          *)
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

(** This module transforms types from the user's syntax to the
    internal representation of the inference engine. *)

open Positions
open Misc
open KindInferencer
open Constraint
open TypeAlgebra
open InferenceTypes
open MultiEquation
open TypingEnvironment
open InferenceExceptions
open Types
open Name
open IAST

let rec extract_type = function

  | ETypeConstraint (_, e, typ) ->
      typ, e

  | ELambda (pos, (p, Some typ1), e2) ->
    let typ2, e2 = extract_type e2 in
    (TyApp (pos, TName "->", [typ1; typ2]),
     ELambda (pos, (p, None), e2))

  | _ ->
      raise Not_found

type recursive_value_definition_kind =
  | Implicit of name * expression
  | Explicit of name * Types.t * expression
  | NotPVar

(** [explicit_or_implicit] examines a value definition and determines whether
    it carries an explicit type annotation. It optionally checks that the
    left-hand side is a variable. *)
let rec explicit_or_implicit pos b e =
  match b with
    | (name, Some typ) ->
      explicit_or_implicit pos (name, None) (ETypeConstraint (pos, e, typ))

    | (name, None) -> (
        try
          let typ, e = extract_type e in
          Explicit (name, typ, e)
        with Not_found ->
          Implicit (name, e)
      )

(** {2 From user's syntax to internal term representation} *)

let variables_of_typ =
  let rec vtyp accu = function
    | TyVar (_, TName x) ->
        StringSet.add x accu

    | TyApp (_, _, ts) ->
        List.fold_left vtyp accu ts
  in
    vtyp StringSet.empty

let arrow tenv =
  arrow (typcon_variable tenv)

let rec type_of_args t =
  let rec chop acu = function
    | TyApp (_, TName "->", [ t1; t2 ]) ->
        chop (t1 :: acu) t2

    | t ->
        acu
  in List.rev (chop [] t)

let arity t =
  List.length (type_of_args t)

let tycon tenv t =
  app (lookup_type_variable tenv t)

let rec intern' pos tenv ty =
  match ty with
    | TyVar (pos, name) ->
      as_fun tenv name

    | TyApp (pos, t, typs) ->
      let iargs = List.map (intern' pos tenv) typs in
      app (as_fun tenv t) iargs

(** [intern tenv typ] converts the type expression [typ] to a type.
    The environment [tenv] maps type identifiers to types. *)
let rec intern pos tenv ty =
  let kind_env = as_kind_env tenv in
  let _ = KindInferencer.check pos kind_env ty star in
    intern' pos tenv ty

let intern_let_env pos tenv rs fs =
  let fqs, rtenv = fresh_flexible_vars pos tenv fs in
  let rqs, rtenv' = fresh_rigid_vars pos tenv rs in
    rqs, fqs, add_type_variables (rtenv @ rtenv') tenv

let intern_class_predicate pos tenv (ClassPredicate (k, a)) =
  let x = variable Flexible () in
  let tx = TVariable x in
  let kty = intern pos tenv (TyVar (pos, a)) in
  let c = (tx =?= kty) pos in
  ((x, c), (k, x))

let intern_class_predicates pos tenv cs =
  let xcs, gs = List.(split (map (intern_class_predicate pos tenv) cs)) in
  let xs, cs = List.split xcs in
  (xs, gs, cs)

(** [intern_scheme tenv name qs cs typ] produces a type scheme
    that binds [name] to [forall qs [cs]. typ]. *)
let intern_scheme pos tenv name qs cs typ =
  let fqs, rtenv = fresh_flexible_vars pos tenv qs in
  let tenv' = add_type_variables rtenv tenv in
  let (xs, gs, cs) = intern_class_predicates pos tenv' cs in
  Scheme (pos, [], xs @ fqs, gs, conj cs,
          StringMap.singleton name (intern pos tenv' typ, pos))

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

open Name
open Position
open Misc
open MultiEquation
open InferenceTypes

(** [sname] is the type of the names that are used to refer to type
    schemes inside constraints. These names are bound by [CLet]
    constraints and referred to by [CInstance] constraints. *)
type sname = SName of string

(** [type_constraint] defines a syntax for the constraints between
    types. *)
type ('crterm, 'variable) type_constraint =
  | CTrue of position
  | CDump of position
  | CPredicate of position * tname * 'crterm
  | CEquation of position * 'crterm * 'crterm
  | CConjunction of ('crterm, 'variable) type_constraint list
  | CLet of
      ('crterm, 'variable) scheme list
    * ('crterm, 'variable) type_constraint
  | CInstance of position * sname * 'crterm
  | CDisjunction of ('crterm, 'variable) type_constraint list

(** A type scheme is a pair of a constraint [c] and a header [h],
    wrapped within two sets of universal quantifiers [rqs] and
    [fqs]. The former are considered rigid, while the latter are
    considered flexible. More precisely, for the type scheme to be
    considered consistent, the constraint [forall rqs.exists fqs.c]
    must hold. Rigid and flexible quantifiers otherwise play the same
    role, that is, they all end up universally quantified in the type
    scheme. A header is a mapping of names to types. *)
and ('crterm, 'variable) scheme =
  | Scheme of position
    * 'variable list
    * 'variable list
    * ('crterm, 'variable) canonical_constraint
    * ('crterm, 'variable) type_constraint
    * ('crterm * position) StringMap.t

and ('crterm, 'variable) canonical_constraint =
    (tname * 'variable) list

type variable = MultiEquation.variable

type variable_kind = MultiEquation.variable_kind

type crterm =
    variable arterm

type tconstraint =
    (crterm, variable) type_constraint

type tscheme =
    (crterm, variable) scheme

type tclass_constraint =
    (crterm, variable) canonical_constraint

let rec expand_term = function
  | App (l, r) ->
      TTerm (map (fun v -> TVariable v ) (App (l, r)))

  | _ -> assert false

let rec expand_term_in_depth t =
  let expand v =
    let desc = UnionFind.find v in
      match desc.structure with
        | None -> TVariable v
        | Some t -> expand_term_in_depth t
  in
    TTerm (map expand t)

let rec cposition = function
  | CTrue pos
  | CDump pos
  | CInstance (pos, _, _)
  | CEquation (pos, _, _)
  | CPredicate (pos, _, _) ->
    pos

  | CLet ([], c) ->
      cposition c

  | (CConjunction [] | CDisjunction []) ->
      dummy

  | (CConjunction l | CDisjunction l) ->
      join (cposition (List.hd l)) (cposition (last l))

  | CLet (l, _) ->
      join (sposition (List.hd l)) (sposition (last l))

and sposition = function
  | Scheme (p, _, _, _, _, _) ->
      p

(** [x <? t] is an instance constraint. *)
let (<?) x t pos =
  CInstance (pos, x, t)

(** [t1 =?= t2] is an equality constraint. *)
let (=?=) t1 t2 pos =
  CEquation (pos, t1, t2)

(** [c1 ^ c2] is a conjunction constraint. *)
let (^) c1 c2 =
  match c1, c2 with
    | CTrue _, c
    | c, CTrue _ ->
      c
    | c, CConjunction cl ->
        CConjunction (c :: cl)
    | _, _ ->
        CConjunction [c1; c2]

let conj cs =
  List.fold_left ( ^ ) (CTrue dummy) cs

(** [ex qs c] returns the constraint [exists qs.c]. We encode existential
   constraints in terms of [let] constraints, since the latter are more
   general. *)
let ex ?pos qs c =
  let ctrue = CTrue (pos_or_undef pos) in
  CLet ([ Scheme (pos_or_undef pos, [], qs, [], c, StringMap.empty) ], ctrue)

(** [fl qs h c] returns the constraint [forall qs [h].c]. We encode
    universal constraints in terms of [let] constraints, since the
    latter are more general. *)
let fl ?pos qs ?(h = []) c =
  let ctrue = CTrue (pos_or_undef pos) in
  CLet ([ Scheme (pos_or_undef pos, qs, [], h, c, StringMap.empty) ], ctrue)

(** [exists f] creates a fresh variable [v] and returns the constraint
    [exists v.(f v)]. *)
let exists ?pos f =
  let v = variable Flexible () in
  let c = f (TVariable v) in
  ex ~pos:(pos_or_undef pos) [ v ] c

let exists3 ?pos f =
  exists (fun x -> exists (fun y -> exists (fun z -> f x y z)))

(** [exists_list l f] associates a fresh variable with every element
    in the list [l], yielding an association list [m], and returns
    the constraint [exists m.(f m)]. *)
let exists_list ?pos l f =
  let l, m = variable_list Flexible l in
  ex ?pos:pos l (f m)

(** [forall_list l f] associates a fresh variable with every element
    in the list [l], yielding an association list [m], and returns
    the constraint [forall m.(f m)]. *)
let forall_list ?pos l f =
  let l, m =
    List.fold_right (fun x (vs, xts) ->
                       let v = variable Rigid ~name:x () in
                         v :: vs, (x, TVariable v) :: xts
                    ) l ([], [])
  in
  fl ~pos:(pos_or_undef pos) l (f m)

(** [exists_set names f] associates a fresh variable with every name in
    the set [names], yielding a map [m] of names to variables, and returns
    the constraint [exists m.(f m)]. *)
let exists_set ?pos names f =
  let l, m = variable_set (const (Flexible, None)) names in
  ex ~pos:(pos_or_undef pos) l (f m)

(** [monoscheme header] turns [header] into a monomorphic type scheme. *)
let monoscheme ?pos header =
  let ctrue = CTrue (pos_or_undef pos) in
  Scheme (pos_or_undef pos, [], [], [], ctrue, header)

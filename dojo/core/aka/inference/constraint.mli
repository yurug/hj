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

(** Syntax for typing constraints. *)

open Misc
open Position
open MultiEquation
open InferenceTypes
open Name

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
  | CLet of ('crterm, 'variable) scheme list
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
    * 'variable list                            (* Rigid variables. *)
    * 'variable list                            (* Flexible variables. *)
    * ('crterm, 'variable) canonical_constraint (* "Given" constraint.  *)
    * ('crterm, 'variable) type_constraint      (* Inferred constraint. *)
    * ('crterm * position) StringMap.t

and ('crterm, 'variable) canonical_constraint =
    (tname * 'variable) list

(** The variables that appear in contraints are the same as the multi-equation
    ones. *)
type variable = MultiEquation.variable

(** The types in contraints are implemented using the internal data structure
    defined in {!InferenceTypes}. The same data structure is also used in
    {!MultiEquation}. *)
type crterm = variable InferenceTypes.arterm

(** Here is an abbreviation for the type constraint structure instantiated using
    our internal variable and term representations. *)
type tconstraint = (crterm, variable) type_constraint

(** Here is an abbreviation for the type scheme structure instantiated using
    out internal variable and term representations. *)
type tscheme = (crterm, variable) scheme

(** Here is an abbreviation for the internal representation of type class
    constraints. *)
type tclass_constraint = (crterm, variable) canonical_constraint

(** [cposition c] returns the position related to [c]. *)
val cposition : ('a, 'b) type_constraint -> Position.position

(** [t1 =?= t2] is an equality constraint *)
val (=?=): crterm -> crterm -> position -> tconstraint

(** [ex qs c] returns the constraint [exists qs.c]. We encode existential
   constraints in terms of [let] constraints, since the latter are more
   general. *)
val ex : ?pos:position -> variable list -> tconstraint -> tconstraint

(** [fl qs c] returns the constraint [forall qs.c]. We encode universal
   constraints in terms of [let] constraints, since the latter are more
   general. *)
val fl: ?pos:position -> variable list -> tconstraint -> tconstraint

(** [x <? t] is a conjunction constraint. *)
val (<?): sname -> crterm -> position -> tconstraint

(** [c1 ^ c2] is a conjunction constraint. *)
val (^): tconstraint -> tconstraint -> tconstraint

(** [conj cs] builds a conjunction between a list of constraints. *)
val conj: tconstraint list -> tconstraint

(** [exists f] creates a fresh variable [x] and returns the constraint
    [exists x.(f x)]. *)
val exists: ?pos:position -> (crterm -> tconstraint) -> tconstraint

(** [exists3 f] is a shortcut for
    [exists (fun x -> exists (fun y -> exists (fun z -> f x y z)))]. *)
val exists3: ?pos:position ->
  (crterm -> crterm -> crterm -> tconstraint) -> tconstraint

(** [fl vs c] returns the constraint [forall vs.c]. *)
val fl: ?pos:position
  -> variable list -> ?h:tclass_constraint -> tconstraint -> tconstraint

(** [exists_list l f] associates a fresh variable with every element
    in the list [l], yielding an association list [m], and returns
    the constraint [exists m.(f m)]. *)
val exists_list:
  ?pos:position -> 'a list -> (('a * crterm) list -> tconstraint)
  -> tconstraint

(** [forall_list l f] associates a fresh variable with every element
    in the list [l], yielding an association list [m], and returns
    the constraint [forall m.(f m)]. *)
val forall_list:
  ?pos:position -> tname list -> ((tname * crterm) list -> tconstraint)
  -> tconstraint

(** [exists_set names f] associates a fresh variable with every name in
    the set [names], yielding a map [m] of names to variables, and returns
    the constraint [exists m.(f m)]. *)
val exists_set: ?pos:position -> StringSet.t ->
  ((crterm * position) StringMap.t -> tconstraint) -> tconstraint

(** [monoscheme header] turns [header] into a monomorphic type scheme. *)
val monoscheme: ?pos:position -> (crterm * position) StringMap.t -> tscheme

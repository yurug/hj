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

(** This module implements a data structure for multi-equations. *)

open Misc
open Position
open Name

(** {3 Multi-equation descriptor} *)

(** The structure of the terms manipulated by a unifier is fixed and
    made visible to the outside, because it must be accessible to
    the constraint solver, which is built on top of a unifier.

    The unifier relies on a union/find algorithm. A variable is
    represented as a point of the union/find algorithm.  An
    equivalence class of points corresponds, roughly speaking, to
    what is called a ``standard multi-equation'' in the book. Every
    multi-equation carries a descriptor. *)
type variable = descriptor UnionFind.point

(** A descriptor contains several pieces of information, the most
    important of which is the structure of the multi-equation, or,
    in other words, its unique non-variable member, if there is one.

    If the [structure] field is [None], then the multi-equation only
    has variable members. If it is [Some t], then the multi-equation
    contains a non-variable member, namely the term [t]. Note that
    [t] is a term whose head symbol belongs to the algebra and whose
    parameters are again variables. Thus, the unifier works with
    so-called ``small terms'' only.

    The [rank] field contains the rank currently attached to the
    multi-equation. As far as the unifier is concerned, ranks have
    no meaning. The unifier only knows that ranks are totally
    ordered. When two multi-equations are fused, the smaller rank is
    kept.

    The [mark] field is transient, and may be used by the unifier's
    client for any purpose. *)
and descriptor = {
  mutable structure: structure option;
  mutable rank: IntRank.t;
  mutable mark: Mark.t;
  mutable kind: variable_kind;
  mutable name: tname option;
  mutable pos: position option;
  mutable var: variable option
}

(** A multi-equation can contain at most one term. In that case, it is
    said to be structured. *)
and structure = variable InferenceTypes.term

(** There are two kinds of variable. A [Flexible] variable can be
    unified with other variables or terms. A [Rigid] variable cannot.*)
and variable_kind = Rigid | Flexible | Constant

(** The type of term of arbitrary depth. *)
type crterm = variable InferenceTypes.arterm

(** [is_structured v] tests if [v] is related to a term. *)
val is_structured : variable -> bool

(** [are_equivalent v1 v2] tests if [v1] and [v2] are in the same
    multi-equation. *)
val are_equivalent : variable -> variable -> bool

(** [variable_name v] returns the name of [v] if it exists. *)
val variable_name : variable -> tname option

(** [variable_structure v] returns the structure of [v] if it exists. *)
val variable_structure : variable -> structure option

(** [explode t] converts an arbitrary depth tree into a 1-depth one using
    variables by introducing fresh intermediate variables. *)
val explode : crterm -> variable InferenceTypes.term

(** [variable ()] returns a fresh variable. *)
val variable: variable_kind -> ?name:tname -> ?structure:crterm ->
  ?pos:position -> unit -> variable

(** [variable_list xs] allocates a fresh variable for every element in the
    list [xs], and returns both a list of these variables and an association
    list that maps elements to variables, viewed as types. *)
val variable_list: variable_kind -> 'a list
  -> variable list * ('a * (crterm)) list

(** [variable_list_from_strings f xs] allocates a fresh variable for every
  string in the list [xs], and returns both a list of these variables
  and an association list that maps elements to variables, viewed as types.
  The kind is determined using the provided function [f]. *)
val variable_list_from_names:
  (tname -> variable_kind * tname option) -> tname list
  -> variable list * (tname * crterm) list

(** [variable_set xs] allocates a fresh variable for every element in the
    set [xs], and returns both a list of these variables and a map of
    elements to variables. *)
val variable_set: (tname -> variable_kind * tname option)
  -> StringSet.t -> variable list * (crterm * position) StringMap.t

(** [is_rigid v] returns true if [v] is a constant or rigid variable. *)
val is_rigid : variable -> bool

(** [is_flexible v] returns true if [v] is a flexible variable. *)
val is_flexible : variable -> bool

(** {3 Pool management}
    The variables are also partitioned into distinct pools. The variable
    pools are related to variable binding location. *)

(** [pool] is an abstract type denoting a set of type variables related
    to a variable binding location. *)
type pool

(** [inhabitants p] returns the type variables of a pool. *)
val inhabitants : pool -> variable list

(** [number p] returns the rank of a [p]. *)
val number : pool -> int

(** [new_pool p] introduces a new pool with a rank equals to the one of
    [p] + 1. *)
val new_pool : pool -> pool

(** [init ()] returns a fresh pool. *)
val init : unit -> pool

(** [register p v] adds [v] into the pool [p] without modifying the
    rank of [v]. *)
val register : pool -> variable -> unit

(** [introduce p v] registers [v] into [p] and updates its rank
    accordingly. *)
val introduce : pool -> variable -> unit

(** [instance p v] returns a valid instance of [v]. *)
val instance : pool -> variable list -> variable list

(** [chop p t] introduces [t] into [p] by registering a variable into [p]
    for each node of its tree maintaining its structure using links between
    these variables. *)
val chop : pool -> crterm -> variable

(** [chopi rank term] chops a term. Any freshly created variables
    receive rank [rank], but are not added to any pool. *)
val chopi : IntRank.t -> crterm -> variable

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

(** This module provides a solver for equations involving set constants,
   variables, and disjoint sums (i.e. unions) thereof. *)

module type SetType =
sig
  type t
    (** The type of sets. *)

  val empty: t
    (** [empty] is the empty set. *)

  val union: t -> t -> t
    (** [union] returns the union of two sets, which may safely be assumed
        disjoint. *)

  val inter: t -> t -> t
    (** [inter] returns the intersection of two sets. *)

  val diff: t -> t -> t
    (** [diff] returns the difference of two sets. *)

  val is_empty: t -> bool
    (** [is_empty] tells whether a set is empty. *)

  val equal: t -> t -> bool
    (** [equal] tells whether two sets are equal. *)

  val print: t -> string
       (** [print s] provides a textual representation of the set [s]. *)

end

module Make (Set : SetType) :
sig

  (** A [term] denotes a disjoint sum of sets with unification
      variables inside. *)
  type term

  (** [variable s] returns a fresh unification variable whose
      denotation cannot intersect [s]. *)
  val variable : Set.t -> term

  (** [svariable ()] is equivalent to [variable Set.empty]. *)
  val svariable : unit -> term

  (** [empty] denotes the constant empty set. *)
  val empty : term

  (** [sum s t] adds [s] to the sum denoted by [t].
      [Error] is raised if [s] intersects [t]. *)
  val sum : Set.t -> term -> term

  (** [unify t t'] solves the equality between two disjoint sums
      by determining the unification variable if necessary.
      If the equality is not satisfiable, [Error] is raised. *)
  val unify : term -> term -> unit

  (** [Error] is raised if the construction of a disjoint sum has
      failed. *)
  exception Error

  (** [print t] returns a string representation of the disjoint
      sum [t]. *)
  val print : term -> string

end

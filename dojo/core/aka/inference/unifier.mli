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

(** This module implements unification of (ranked) multi-equations
    over a row algebra, that is, an algebra obtained by extending a
    free algebra [A] with rows (see module {!CoreAlgebra}).

    For the purposes of this module, a rank is an element of an
    arbitrary total order. A rank is associated with every
    multi-equation. When two multi-equations are merged, the smaller
    rank is kept.

    It is understood that finite and infinite terms are legal -- no
    occur check is performed here. *)

open Positions
open MultiEquation
open InferenceTypes

exception CannotUnify of position * variable * variable

(** [unify register v1 v2] equates the variables [v1] and [v2]. That
    is, it adds the equation [v1 = v2] to the system of equations
    which [v1] and [v2] are already implicitly part of. Then, it
    rewrites the system of equations in a number of ways until an
    inconsistency is found or a standard (satisfiable) form is
    reached. In the former case, the exception [Inconsistency] is
    raised. In the latter case, the function returns normally,
    without returning a result.

    Every variable that is newly allocated during the process is
    passed to [register], so as to make the unifier's client aware
    of its existence. The variable's rank is already properly
    initialized when [register] is called. *)
val unify: position -> (variable -> unit) -> variable -> variable -> unit

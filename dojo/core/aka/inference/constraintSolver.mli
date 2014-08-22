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

(** This module provides a constraint solver based on unification
    under a mixed prefix. *)

open Positions
open Name
open MultiEquation
open InferenceTypes
open Constraint

(** The constraint to solve. *)
type tconstraint = (crterm, variable) type_constraint

type answer

(** [solve c] solves [c] and returns an answer describing the types
    assigned to program identifiers and how program identifiers' type
    schemes are instantiated at each of their occurrences. This
    function mutates the unifications variables using in [c]. *)
val solve: tconstraint -> answer

(** [lookup_binding a b] returns an explicitly typed version of the
    binding [b] as described by the answer [a]. *)
val lookup_binding
  : answer -> string -> (variable list * (tname * variable) list * variable)

(** An occurrence is an identifier and a position in the source. *)
type occurrence = string * position

(** [lookup_instantiation a o] returns the type to which the type
    scheme of a variable [x] has been instantiated at its occurrence
    [o]. *)
val lookup_instantiation : answer -> occurrence -> variable

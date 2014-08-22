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

(** This module exports the internal terms maintained by the unifier
    into terms of the external syntax of types and type schemes. *)

open Name
open Positions
open MultiEquation

exception RecursiveType of position

(** [type_of_variable pos x] returns an external representation of the
    object [x] as a type. Consecutive calls to [print] share the same
    variable naming conventions, unless [reset] is called in
    between. *)
val type_of_variable: position -> variable -> Types.t

(** [type_of_variable pos x] returns an external representation of the
    object [x] as a type scheme. Consecutive calls to [print] share
    the same variable naming conventions, unless [reset] is called in
    between. *)
val type_scheme_of_variable
  : position
  -> (variable list * (tname * variable) list * variable) -> Types.scheme

(** [reset ()] clears the memoization table to maintain naming conventions. *)
val reset: unit -> unit

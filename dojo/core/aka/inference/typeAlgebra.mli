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

(** This module provides the type algebra for the ML language. *)
open Position

(** The type algebra augments the {!CoreAlgebra} to relate it with
    the ML source language. *)
open InferenceTypes
open Name

(** Head symbols. *)
type symbol

(** Associativity of a symbol. *)
type associativity =
    AssocLeft
  | NonAssoc
  | AssocRight
  | EnclosedBy of string * string

(** [as_symbol s] maps the string [s] to a symbol if [s] is a
    valid symbol name. *)
val as_symbol: tname -> symbol option

(** [associativity s] returns the associativity of [s]. *)
val associativity: symbol -> associativity

(** [priority s] returns the priority of [s]. *)
val priority: symbol -> int

(** [infix s] tests if [s] is infix. *)
val infix: symbol -> bool

(** A type constructor is a type variable with higher-order kind.
    It is introduced as any type variable in the multi-equation set
    during the constraint generation. Then, an environment is given to
    the algebra in order to retrieve the type variable associated
    to the string representation of the type constructor. *)
type 'a environment = tname -> 'a arterm

(** [arrow env t1 t2] return the type [t1 -> t2]. *)
val arrow : 'a environment -> 'a arterm -> 'a arterm -> 'a arterm

(** [arrow env ts] returns the type [t0 -> ... -> tn]. *)
val n_arrows: 'a environment -> 'a arterm list -> 'a arterm -> 'a arterm

(** [result_type env t] returns the result type of the type [t] if
    [t] is an arrow type. *)
val result_type :  'a environment -> 'a arterm -> 'a arterm

(** [result_type env t] returns the argument types of the type [t] if
    [t] is an arrow type. *)
val arg_types : 'a environment -> 'a arterm -> 'a arterm list

(** [tycon_args ty] returns the actual arguments of the type
    application if [ty] is a type application. It returns the
    empty list otherwise. *)
val tycon_args : 'a arterm -> 'a arterm list

(** [tycon_name ty] returns the name of the type constructor
    of [ty]. Precondition: [ty] has kind [KStar]. *)
val tycon_name : 'a arterm -> 'a arterm

(** [type_of_primitive p] returns the type of a source language primitive. *)
val type_of_primitive : 'a environment -> IAST.primitive -> 'a arterm

(** The type of predefined data constructors. *)
type builtin_dataconstructor = dname * tname list * Types.t

(** [init_builtin_env variable_maker] uses [variable_maker] to built
    a typing environment that maps type constructor names to their arities
    and their type variables. *)
val init_builtin_env: (?name:tname -> unit -> 'a)
  -> (tname * (Types.kind * 'a arterm * builtin_dataconstructor list)) list

val builtin_env:
  (tname
   * (bool * associativity * int * Types.kind
      * (dname * tname list * Types.t) list))
  array

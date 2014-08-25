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

(** [TypingEnvironment] implements two mappings used during constraint
    generation:

    - The first one associates a kind, a flexible variable and an optional
    list of data constructor to a type name.

    - The second one records the scheme of the data constructors. *)

open MultiEquation
open Position
open Name

(** A type constructor is characterized by an arity, a variable and an
    optional set of algebraic data constructors. *)
type type_info = KindInferencer.t * variable * datatype_info ref

and datatype_info =
  | Abstract
  | Sum     of (dname * variable) list
  | Product of variable list * crterm * (lname * crterm) list

(** Each data constructor is assigned an ML scheme. *)
type data_constructor = int * variable list * crterm

(** The type of the typing environement. *)
type environment

(** The empty environment. *)
val empty_environment : environment

(** [fold_type_info] folds over the environment focusing on type's
    information. *)
val fold_type_info: ('a -> (tname * type_info) -> 'a) -> 'a -> environment -> 'a

(** Add a set of type variables into the environment, associating a
    name to each. *)
val add_type_variables: (tname * type_info) list -> environment -> environment

(** Add a type constructor into the environment. *)
val add_type_constructor: environment -> tname -> type_info -> environment

(** Add a data constructor into the environment. *)
val add_data_constructor:
  environment -> dname -> data_constructor -> environment

(** [is_regular_datacon_scheme env vs ty] checks that forall vs.ty is
    a valid scheme for a data constructor that is to say following the
    shape:
    K :: forall a1 .. an. tau_1 -> ... -> tau_n -> eps a1 ... an *)
val is_regular_datacon_scheme: environment -> variable list -> crterm -> bool

(** [lookup_datacon env k] gives access to the typing information
    related to the data constructor [k] in [env]. *)
val lookup_datacon
  : ?pos:Position.position -> environment -> dname -> data_constructor

(** Looks for a type constructor given its name. *)
val lookup_type_variable :
  ?pos:Position.position -> environment -> tname
  -> variable InferenceTypes.arterm

(** Accessor to the arity of a type constructor. *)
val typcon_kind : environment -> tname -> KindInferencer.t

(** Accessor the unification variable of a type. *)
val typcon_variable : environment -> tname -> variable InferenceTypes.arterm

(** [as_fun env] provides a view of [env] as function from names to
    variable. This is used to abstract the environment when it is
    given to the {!MiniAlgebra} module
    (see {!MiniAlgebra.type_of_primitive} for instance). *)
val as_fun : environment -> (tname -> variable InferenceTypes.arterm)

(** [as_kind env] provides a view of [env] as kind environment. *)
val as_kind_env : environment ->
  (tname -> KindInferencer.t) * (tname -> KindInferencer.t -> unit)

(** [fresh_datacon_scheme env dname vs] retrieves the type scheme
    of [dname] in [env] and alpha convert it using [vs] as a set
    of names to use preferentially when printing. *)
val fresh_datacon_scheme :
  position -> environment -> dname -> (variable list * crterm)

(** [fresh_flexible_vars pos env vs] returns a list of fresh flexible
    variables whose visible names are [vs] and an environment fragment. *)
val fresh_flexible_vars :
  position -> environment -> tname list ->
  variable list * (tname * type_info) list

(** [fresh_flexible_vars pos env vs] returns a list of fresh rigid
    variables whose visible names are [vs] and an environment fragment. *)
val fresh_rigid_vars :
  position -> environment -> tname list ->
  variable list * (tname * type_info) list

(** [fresh_flexible_vars pos env] returns a list of fresh rigid
    variables without visible names and an environment fragment. *)
val fresh_unnamed_rigid_vars :
  position -> environment -> 'a list -> variable list * ('a * type_info) list

(** Merge a environment fragment with an environment. *)
val add_type_and_kind_variables :
  (tname * variable) list -> environment -> environment

(** [fresh_product_of_label env l] returns the type of the record
    type containing [l] as well as the type of its labels. In addition,
    the type parameters of the record type are taken fresh. *)
val fresh_product_of_label : position -> environment -> lname
  -> variable list * (crterm * (lname * crterm) list)

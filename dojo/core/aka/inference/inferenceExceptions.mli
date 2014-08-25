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

(** This modules declares the exceptions raised by the type inference engine. *)

open Name
open InferenceTypes
open MultiEquation
open Position

(** [IncompatibleTypes] is raised when two types do not unify. *)
exception IncompatibleTypes of position * variable * variable

(** [TypingError] is raised when an inconsistency is detected during
    constraint solving. *)
exception TypingError of position

(** [UnboundTypeIdentifier] is raised when an unbound type identifier
    is found. *)
exception UnboundTypeIdentifier of position * tname

(** [UnboundIdentifier] is raised when an unbound identifier
    is found. *)
exception UnboundIdentifier of position * name

(** [InvalidTypeVariableIdentifier] is raised when a type variable is
    overwriting a type constructor. *)
exception InvalidTypeVariableIdentifier of position * tname

(** [UnboundDataConstructor] is raised when a constructor identifier is
  used although it has not been defined. *)
exception UnboundDataConstructor of position * dname

(** [InvalidDataConstructorDefinition] is raised when the declared
    type scheme of a data constructor is not regular. *)
exception InvalidDataConstructorDefinition of position * dname

(** [UnboundTypeVariable] is raised when a variable identifier is
    used although it has not been defined. *)
exception UnboundTypeVariable of position * tname

(** [MultipleLabels] is raised when the user has built a record
    with two fields with the same name. *)
exception MultipleLabels of position * lname

(** [UnboundLabel] is raised when the user refers to an undeclared
    label. *)
exception UnboundLabel of position * lname

(** [NonLinearPattern] is raised when at least two occurrences of a variable
    appear in a pattern. *)
exception NonLinearPattern of position * name

(** [InvalidDisjunctionPattern] is raised when the subpatterns of a
    disjunction pattern do not bind the same variables. *)
exception InvalidDisjunctionPattern of position

(** [NotEnoughPatternArgts] is raised when the arity of a data constructor
    is not respected in a pattern. *)
exception NotEnoughPatternArgts of position

(** [CannotGeneralize] when the type of an expression cannot be
    generalized contrary to what is specified by the programmers
    using type annotations. *)
exception CannotGeneralize of position * variable

(** [NonDistinctVariables] is raised when two rigid type variables have
    been unified. *)
exception NonDistinctVariables of position * (variable list)

(** [UnboundConstructor] is raised when a type constructor is unbound. *)
exception UnboundTypeConstructor of position * tname

(** [KindError] is raised when the kind of types are not correct. *)
exception KindError of position

(** [PartialDataConstructorApplication] is raised when a data constructor's
    arity is not respected by the programmer. *)
exception PartialDataConstructorApplication of position * int * int

(** [UnboundClass] is raised when a class identifeir is used even
    though it has not been defined. *)
exception UnboundClass of position * tname

(** [MultipleClassDefinition] is raised when a class identifeir is
    defined several times. *)
exception MultipleClassDefinitions of position * tname

(** [OverlappingInstance] is raised when two instances of the
    same class overlap. *)
exception OverlappingInstances of position * tname * variable

(** [InvalidClassPredicateInContext] is raised when a class is applied to
    something that is not a variable, in a typing context. *)
exception InvalidClassPredicateInContext of position * tname

(** [IncompatibleLabel] is raised when a label is used in a
    record expression whose type does not contain it. *)
exception IncompatibleLabel of position * lname

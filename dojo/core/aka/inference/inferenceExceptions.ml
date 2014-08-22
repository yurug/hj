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
open MultiEquation
open Positions

exception UnboundTypeIdentifier of position * tname

exception InvalidTypeVariableIdentifier of position * tname

exception UnboundDataConstructor of position * dname

exception InvalidDataConstructorDefinition of position * dname

exception UnboundTypeVariable of position * tname

exception MultipleLabels of position * lname

exception UnboundLabel of position * lname

exception NonLinearPattern of position * name

exception NotEnoughPatternArgts of position

exception CannotGeneralize of position * variable

exception NonDistinctVariables of position * (variable list)

exception UnboundIdentifier of position * tname

exception UnboundTypeConstructor of position * tname

exception KindError of position

exception RecursiveDefMustBeVariable of position

exception InvalidDisjunctionPattern of position

exception PartialDataConstructorApplication of position * int * int

exception MultipleClassDefinitions of position * tname

exception UnboundClass of position * tname

exception IncompatibleTypes of Positions.position * variable * variable

exception TypingError of Positions.position

exception UnboundIdentifier of position * name

exception OverlappingInstances of position * tname * variable

exception InvalidClassPredicateInContext of position * tname

exception IncompatibleLabel of position * lname

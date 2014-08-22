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

(** This module implements a simple and efficient union/find algorithm.
    See Robert E. Tarjan, ``Efficiency of a Good But Not Linear Set
    Union Algorithm'', JACM 22(2), 1975. *)

(** The abstraction defined by this module is a set of points,
    partitioned into equivalence classes. With each equivalence class,
    a piece of information, of abstract type ['a], is associated; we
    call it a descriptor. *)
type 'a point

(** [fresh desc] creates a fresh point and returns it. It forms an
    equivalence class of its own, whose descriptor is [desc]. *)
val fresh: 'a -> 'a point

(** [find point] returns the descriptor associated with [point]'s
    equivalence class. *)
val find: 'a point -> 'a

(** [union point1 point2] merges the equivalence classes associated
    with [point1] and [point2] (which must be distinct) into a single
    class whose descriptor is that originally associated with [point2]. *)
val union: 'a point -> 'a point -> unit

(** [equivalent point1 point2] tells whether [point1] and [point2]
    belong to the same equivalence class. *)
val equivalent: 'a point -> 'a point -> bool

(** [redundant] maps all members of an equivalence class, but one, to
    [true]. *)
val redundant: 'a point -> bool

(** [change p d] updates the descriptor of [p] to [d]. *)
val change: 'a point -> 'a -> 'a

val id : 'a point -> int

val repr : 'a point -> 'a point

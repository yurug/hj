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

(** This module implements infinite arrays. **)
type 'a t

(** [make x] creates an infinite array, where every slot contains [x]. **)
val make: 'a -> 'a t

(** [get a i] returns the element contained at offset [i] in the array [a].
   Slots are numbered 0 and up. **)
val get: 'a t -> int -> 'a

(** [set a i x] sets the element contained at offset [i] in the array
    [a] to [x]. Slots are numbered 0 and up. **)
val set: 'a t -> int -> 'a -> unit

(** [iteri f t] computes [f i x] on every element [x] at position [i]
    in [t]. *)
val iteri: (int -> 'a -> unit) -> 'a t -> unit

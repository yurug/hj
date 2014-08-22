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

(** Environment who maps keys whose type is ['a] to values of type ['b]. *)
type ('a, 'b) t

(** [lookup env k] returns the value associated to [k]
    or raises [Not_found]. *)
val lookup : ('a, 'b) t -> 'a -> 'b

(** [lookup_image env p] returns a binding [(k, v)] such
    that [p v] or raises [Not_found]. *)
val lookup_image : ('a, 'b) t -> ('b -> 'c option) -> 'c

(** [filter env pred] returns the set of values that verify [pred]. *)
val filter : ('a, 'b) t -> ('b -> bool) -> 'b list

(** [empty] is the environment with no binding at all. *)
val empty : ('a, 'b) t

(** [add env k v] associates [v] to [k] in [env]. *)
val add : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t

(** [concat env env'] merges the mapping of [env] and [env']
    preferring the mapping of [env] if there is a clash. *)
val concat : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

(** [iter f env] iterates over the mappings of [env]. *)
val iter : (('a * 'b) -> unit) -> ('a, 'b) t -> unit

(** [fold f init env] iterates over the mappings of [env]
    conveying an accumulator whose value is [init] initially. *)
val fold_left : ('c -> ('a * 'b) -> 'c) -> 'c -> ('a, 'b) t -> 'c

(** [map f env] applies a function [f] on each mapping of [env]
    building a list [f (k, v)]. *)
val map : (('a * 'b) -> 'c) -> ('a, 'b) t -> 'c list

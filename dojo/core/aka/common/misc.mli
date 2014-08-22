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

(** This module contains miscellaneous utilities. *)

(** [iter] is similar to [List.iter], but does not require [f] to
    return a result of type [unit]. Use with caution. *)
val iter: ('a -> 'b) -> 'a list -> unit

(** If [l] is a list of pairs of a key and a datum, and if [p] is a
    predicate on keys, then [assocp p l] returns the datum associated
    with the first key that satisfies [p] in [l]. It raises
    [Not_found] if no such key exists. *)
val assocp: ('a -> bool) -> ('a * 'b) list -> 'b

(** Sets of strings. *)
module StringSet : Set.S with type elt = string

(** Maps over strings. *)
module StringMap : sig

  include Map.S with type key = string

  val singleton: string -> 'a -> 'a t

  exception Strict of string

  val strict_add: key -> 'a -> 'a t -> 'a t

  val union: 'a t -> 'a t -> 'a t

  val strict_union: 'a t -> 'a t -> 'a t

end with type key = string

(** A debugging flag. *)
val debug: bool ref

(** Prints a list of elements, with one occurrence of the separator
    between every two consecutive elements. *)
val print_separated_list: string -> ('a -> string) -> 'a list -> string

(** Returns a pair of function [import] and [export] to assign
  a unique integer to a given value. *)
val make_indexes : 'a -> ('a -> int) * (int -> 'a) * ('a -> int)

(** Returns the last element of a list. Linear complexity. *)
val last : 'a list -> 'a

(**/**)
val curry : ('a -> 'b -> 'c) -> ('a * 'b -> 'c)

val switch_args : ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)

(* FIXME: this should be a camlp4 macro. *)
val safe_find : 'a -> 'a

val notf : ('a -> bool) -> 'a -> bool

val eqf : 'a -> ('a -> bool)

val is_now : 'a option -> 'a option -> 'a option

val itern : int -> (unit -> 'a) -> 'a list

val update : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list

val updatef :  ('b -> 'b) -> 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list

val intersect : StringSet.t -> StringSet.t -> bool

val set_of_list : string list -> StringSet.t

val map_union : 'a StringMap.t -> 'a StringMap.t -> 'a StringMap.t

val twice : ('a -> 'b) -> 'a -> 'a -> ('b * 'b)

exception InvalidOptionUse

val isNonef : ('a -> 'b option) -> ('a -> bool)

val default : 'a -> 'a option -> 'a

val unSome : 'a option -> 'a

val unSomef : ('a -> 'b option) -> ('a -> 'b)

val list_unionq : 'a list -> 'a list -> 'a list

val list_removeq : 'a -> 'a list -> 'a list

val pmapq : ('a * 'a) list -> 'a -> 'a

val const : 'a -> ('b -> 'a)

val array_assoc : 'a -> ('a * 'b) array -> 'b
val array_associ : 'a -> ('a * 'b) array -> int

val assoc_proj1 : ('a * 'b) list -> 'a list
val assoc_proj2 : ('a * 'b) list -> 'b list

val proj1_3 : ('a * 'b * 'c) -> 'a
val proj2_3 : ('a * 'b * 'c) -> 'b
val proj3_3 : ('a * 'b * 'c) -> 'c

val proj1_4 : ('a * 'b * 'c * 'd) -> 'a
val proj2_4 : ('a * 'b * 'c * 'd) -> 'b
val proj3_4 : ('a * 'b * 'c * 'd) -> 'c
val proj4_4 : ('a * 'b * 'c * 'd) -> 'd

val proj1_5 : ('a * 'b * 'c * 'd * 'e) -> 'a
val proj2_5 : ('a * 'b * 'c * 'd * 'e) -> 'b
val proj3_5 : ('a * 'b * 'c * 'd * 'e) -> 'c
val proj4_5 : ('a * 'b * 'c * 'd * 'e) -> 'd
val proj5_5 : ('a * 'b * 'c * 'd * 'e) -> 'e

val split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
val split4 : ('a * 'b * 'c * 'd) list -> 'a list * 'b list * 'c list * 'd list
val split5 :
('a * 'b * 'c * 'd * 'e) list -> 'a list * 'b list * 'c list * 'd list * 'e list

val transpose : ('a list) list -> ('a list) list

val gcombine : 'a list -> 'b list -> ('a * 'b) list * 'a list * 'b list

val list_map_array : ('a -> 'b) -> 'a list -> 'b array

val list_iteri : (int -> 'a -> unit) -> 'a list -> unit

val list_mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

exception NonDisjointCase
exception Failure of exn list
type ('a, 'b) either = Left of 'a | Right of 'b

val one_of : (unit -> 'a) -> (unit -> 'b) -> (('a * exn), ('b * exn)) either

val reraise : (unit -> 'a) -> exn -> exn -> 'a

val just_try : (unit -> 'a) -> 'a option

val ( ^^ ) : string -> string -> string

val opt_apply : ('a -> 'b) option -> 'a -> 'b option

val list_foralli : (int -> 'a -> bool) -> 'a list -> bool
val list_existsi : (int -> 'a -> bool) -> 'a list -> bool
val list_mapi2 : (int -> 'a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val list_foldmap : ('b -> 'a -> 'c * 'b) -> 'b -> 'a list -> 'c list * 'b

val are_distinct: 'a list -> 'a option

val all_equal : 'a list -> bool * 'a option

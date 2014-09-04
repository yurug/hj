(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*s Sets implemented as Red-Black trees.
    Interface copied from Ocaml's [Set]. *)

(* Changes:
   - Add [find] in [S].
   - Have types deriving (Json).
   - Add [Dict], a straightforward implementation of dictionaries.
*)

module type OrderedType =
  sig
    type t deriving (Json)
    val compare : t -> t -> int
  end

module type S =
  sig
    type elt deriving (Json)
    type t deriving (Json)
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val find : (elt -> int) -> t -> elt
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val for_all: (elt -> bool) -> t -> bool
    val exists: (elt -> bool) -> t -> bool
    val filter: (elt -> bool) -> t -> t
    val partition: (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  end

module Make(Ord : OrderedType) : (S with type elt = Ord.t)

module Dict (S : sig
  type key deriving (Json)
  type image deriving (Json)
  val compare : key -> key -> int
end) : sig

  type t deriving (Json)

  val empty : t

  val add : S.key -> S.image -> t -> t

  val update : S.key -> S.image -> t -> t

  val lookup : S.key -> t -> S.image

  module Set : S with type elt = S.key * S.image

  val to_set : t -> Set.t

end

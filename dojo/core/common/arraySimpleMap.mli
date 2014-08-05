module type OrderedByKeyData = sig
  type key
  val compare : key -> key -> int
  type data
  val get_key : data -> key
end

module Make : functor (S : OrderedByKeyData) ->
sig
  type t
  val make : int -> t

  exception InvalidIndex of int
  val get : t -> int -> S.data

  exception Full
  exception MisOrdered of S.data * S.data
  val insert : t -> S.data -> int

  val insert_map : t -> t -> int

  exception Not_found
  val find : t -> S.key -> int * S.data

  val length : t -> int

  val last : t -> S.data

  val last_key : t -> S.key

  val sub : t -> S.key -> S.key -> t

  val empty : t -> bool

  val iter : t -> (S.data -> unit) -> unit

  val rev_iter : t -> (S.data -> unit) -> unit

  type iterator

  val start : t -> iterator

  val value : t -> iterator -> S.data

  val next : iterator -> iterator

  val at_the_end : iterator -> bool

end

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

  exception Not_found
  val find : t -> S.key -> int * S.data

  val length : t -> int

end

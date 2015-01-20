(** This module implements a notion of timestamp. *)

type t deriving (Json)

type timestamp = t deriving (Json)

val current : unit -> t

val origin : unit -> t

val compare : t -> t -> int

val to_string : t -> string

val shift : t -> float -> t

type interval

val make : timestamp -> timestamp -> interval

val mem : timestamp -> interval -> bool

val always : interval

val older_than : timestamp -> timestamp -> bool

val older_than_duration : float -> timestamp -> bool

val from_float : float -> t

val to_float : t -> float

(** This module implements a notion of timestamp. *)

type t

type timestamp = t

val current : unit -> t

val origin : unit -> t

val compare : t -> t -> int

type interval

val make : timestamp -> timestamp -> interval

val mem : timestamp -> interval -> bool

val always : interval

(** This module implements a notion of timestamp. *)

type t

type timestamp = t

val current : unit -> t

val compare : t -> t -> int

(* -*- tuareg -*- *)

(** Logging facilities. *)

type tag =
  | Strace
  | Internal

val log : tag list -> string -> unit

val unexpected_failure : tag list -> (unit -> unit) -> unit

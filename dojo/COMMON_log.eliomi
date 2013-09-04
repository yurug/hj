(* -*- tuareg -*- *)

(** Logging facilities. *)

type tag =
  | Strace

val log : tag list -> string -> unit

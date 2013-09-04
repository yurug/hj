(* -*- tuareg -*- *)

(** Logging facilities. *)

type tag =
  | Strace

let prefix_of_tag = function
  | Strace -> "sys"

let prefix_of_tags ts = String.concat "," (List.map prefix_of_tag ts)

let log ts msg =
  Ocsigen_messages.errlog (Printf.sprintf "%s> %s" (prefix_of_tags ts) msg)

(* -*- tuareg -*- *)

(** Logging facilities. *)

type tag =
  | Strace
  | Internal

let prefix_of_tag = function
  | Strace -> "sys"
  | Internal -> "core"

let prefix_of_tags ts = String.concat "," (List.map prefix_of_tag ts)

let log ts msg =
  Ocsigen_messages.errlog (Printf.sprintf "%s> %s" (prefix_of_tags ts) msg)

let unexpected_failure ts f =
  try
    f ()
  with (Assert_failure _) as e ->
    log ts (Printexc.to_string e)

(* -*- tuareg -*- *)

(** The Hacking Dojo. *)

open HTML_entity
open HTML_exercise

let system_initialization =
  CORE_vfs.init_root ()

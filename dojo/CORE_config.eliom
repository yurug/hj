(* -*- tuareg -*- *)

(** Use the root of the data directory to store our ressources. *)
let ressource_root =
  Filename.concat (Ocsigen_config.get_datadir ()) "root"

(** Check the AUTOTEST environment variable. *)
let autotest_enabled () =
  try ignore (Sys.getenv "AUTOTEST"); true with Not_found -> false

{shared{

(** We only support french for the moment. *)
type language = French
let current_language () = French

}}

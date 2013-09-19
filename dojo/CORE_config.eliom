(* -*- tuareg -*- *)

(** Use the root of the data directory to store our ressources. *)
let absolute_data_dir () =
  let ocs = Ocsigen_config.get_datadir () in
  if Filename.is_relative ocs then
    Filename.concat (Unix.getcwd ()) ocs
  else
    ocs

let ressource_root =
  Filename.concat (absolute_data_dir ()) "root"

(** Ace editor location. *)
let ace_editor_src () =
  let ocs = absolute_data_dir () in
  let lcs = Filename.concat ocs "ace-builds" in
  Ocsigen_messages.errlog lcs;
  Eliom_content_core.Xml.uri_of_string (
    if Sys.file_exists lcs then
      "/ace-builds/src-noconflict/ace.js"
    else
      "https://raw.github.com/ajaxorg/ace-builds/master/src-noconflict/ace.js"
  )

(** Check the AUTOTEST environment variable. *)
let autotest_enabled () =
  try ignore (Sys.getenv "AUTOTEST"); true with Not_found -> false

let number_of_login_attempts_per_second = 2

{shared{

(** We only support french for the moment. *)
type language = French
let current_language () = French

}}

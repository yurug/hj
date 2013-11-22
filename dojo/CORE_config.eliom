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

(** Mathjax location. *)
let mathjax_src () =
  Eliom_content_core.Xml.uri_of_string
    "https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/\
     MathJax.js?config=TeX-AMS-MML_HTMLorMML"

(** Ace editor location. *)
let ace_editor_src () =
  Eliom_content_core.Xml.uri_of_string "/ace-builds/src-noconflict/ace.js"

let ace_alias_src () =
  Eliom_content_core.Xml.uri_of_string "/ace-alias.js"

(** Check the AUTOTEST environment variable. *)
let autotest_enabled () =
  try ignore (Sys.getenv "AUTOTEST"); true with Not_found -> false

let number_of_login_attempts_per_second = 2

{shared{

(** We only support french for the moment. *)
type language = French
let current_language () = French

}}

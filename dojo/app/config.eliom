(* -*- tuareg -*- *)

(** Use the following mailer. *)
let mailer = ref "/usr/sbin/sendmail"
let set_mailer = ( := ) mailer
let get_mailer () = !mailer

(** Use the root of the data directory to store our ressources. *)
let absolute_data_dir () =
  let ocs = Ocsigen_config.get_datadir () in
  if Filename.is_relative ocs then
    Filename.concat (Unix.getcwd ()) ocs
  else
    ocs

(** Mathjax location. *)
let mathjax_src () =
  Eliom_content.Xml.uri_of_string
    "https://cdn.mathjax.org/mathjax/latest/\
     MathJax.js?config=TeX-AMS-MML_HTMLorMML"

(** codemirror editor source location. *)
let codemirror_editor_src () =
  Eliom_content.Xml.uri_of_string "/codemirror/lib/codemirror.js"

let codemirror_editor_modes () = [
  Eliom_content.Xml.uri_of_string "/codemirror/mode/clike/clike.js";
  Eliom_content.Xml.uri_of_string "/codemirror/mode/clike/mllike.js"
]

(** codemirror editor css location. *)
let codemirror_editor_css () =
  ["codemirror"; "lib"; "codemirror.css"]

(** highlight source location. *)
let highlight_src () =
  Eliom_content.Xml.uri_of_string "/highlight/highlight.pack.js"

(** highlight css location. *)
let highlight_css () =
  ["highlight"; "styles"; "default.css"]

(** jQuery source location. *)
let jquery_src () =
  Eliom_content.Xml.uri_of_string "/jquery-2.1.1.min.js"

let number_of_login_attempts_per_second = 2

let size_of_entity_log_history = 128

{shared{

(** We only support french for the moment. *)
type language = French
let current_language () = French

}}

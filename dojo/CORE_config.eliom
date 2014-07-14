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
  Eliom_content.Xml.uri_of_string
    "https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/\
     MathJax.js?config=TeX-AMS-MML_HTMLorMML"

(** codemirror editor source location. *)
let codemirror_editor_src () =
  Eliom_content.Xml.uri_of_string "/codemirror/lib/codemirror.js"

(** codemirror editor css location. *)
let codemirror_editor_css () =
  ["codemirror"; "lib"; "codemirror.css"]

(** Check the AUTOTEST environment variable. *)
let autotest_enabled () =
  try ignore (Sys.getenv "AUTOTEST"); true with Not_found -> false

let number_of_login_attempts_per_second = 2

let size_of_entity_log_history = 128

{shared{

(** We only support french for the moment. *)
type language = French
let current_language () = French

}}

type ldap_configuration = {
  mutable host : string;
  mutable port : int;
  mutable domain : string;
  mutable username : string;
  mutable password : string;
  mutable base : string;
  mutable firstname_field: string;
  mutable name_field: string;
  mutable fullname_field: string;
  mutable login_field: string;
  mutable email_field: string;
  mutable status_field: string;
  mutable teacher_status_value: string;
}

let ldap_servers = ref []

let ldap_configuration = Ocsigen_extensions.Configuration.(
  (** Default configuration *)
  let config () = {
    host = "";
    port = 339;
    base = "";
    domain = "";
    password = "";
    username = "";
    firstname_field = "";
    fullname_field = "";
    name_field = "";
    login_field = "";
    email_field = "";
    status_field = "";
    teacher_status_value = ""
  }
  in
  let init () =
    ldap_servers := config () :: !ldap_servers
  in
  let current () =
    List.hd !ldap_servers
  in

  (** Parsing functions. *)
  let req_attr name = attribute ~name ~obligatory:true
  and opt_attr name = attribute ~name ~obligatory:false
  in
  let name = "LDAP"
  and obligatory = false
  and attributes = [
    req_attr "host" (fun h ->
      Ocsigen_messages.errlog ("Found LDAP server at " ^ h);
      (current ()).host <- h
    );
    req_attr "domain" (fun p -> (current ()).domain <- p);
    opt_attr "port" (fun p -> (current ()).port <- int_of_string p);
    opt_attr "password" (fun p -> (current ()).password <- p);
    opt_attr "username" (fun p -> (current ()).username <- p);
    req_attr "base" (fun h -> (current ()).base <- h);
    req_attr "firstname" (fun h -> (current ()).firstname_field <- h);
    opt_attr "name" (fun h -> (current ()).name_field <- h);
    opt_attr "fullname" (fun h -> (current ()).fullname_field <- h);
    req_attr "login" (fun h -> (current ()).login_field <- h);
    opt_attr "email" (fun h -> (current ()).email_field <- h);
    req_attr "status" (fun h -> (current ()).status_field <- h);
    req_attr "teacher_status_value" (fun h ->
      (current ()).teacher_status_value <- h
    );
  ]
  in
  element ~init ~name ~obligatory ~attributes ()
)

let ldap_servers () = !ldap_servers

let development_mode = ref false

let development_mode_configuration = Ocsigen_extensions.Configuration.(
  let init = fun () -> development_mode := true in
  let name = "development" in
  element ~init ~name ()
)

let development_mode () = !development_mode

let _ =
  Eliom_config.parse_config [
    ldap_configuration;
    development_mode_configuration
  ]

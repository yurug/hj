(* -*- tuareg -*- *)

(** Configuration parameters. *)

(** [ressource_root] is the place where the state of the system is stored. *)
val ressource_root : string

(** [autotest_enabled ()] returns [true] if the server is in
    autotesting mode. This mode is enabled if the environment
    variable [AUTOTEST] is set. *)
val autotest_enabled : unit -> bool

(** [mathjax_src ()] returns the way to import [mathjax]
    in the project. *)
val mathjax_src : unit -> Eliom_content_core.Xml.uri

(** [codemirror_editor_src ()] returns the way to import the
    [codemirror editor] in the project. *)
val codemirror_editor_src : unit -> Eliom_content_core.Xml.uri

(** [codemirror_editor_css ()] returns the way to import the
    [codemirror css] in the project. *)
val codemirror_editor_css : unit -> string list

(** [number_of_login_attempts_per_second] *)
val number_of_login_attempts_per_second : int

(** [size_of_entity_log_history] *)
val size_of_entity_log_history : int

{shared{

(** Supported human languages in the user interface. *)
type language = French

(** [current_language ()] looks in the standard locale environment
    variables to determine in which human language the user interface
    must talk. *)
val current_language : unit -> language

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

val ldap_servers : unit -> ldap_configuration list

val development_mode : unit -> bool

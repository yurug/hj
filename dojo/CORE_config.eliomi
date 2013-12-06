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

(** [ace_editor_src ()] returns the way to import the [ace editor]
    in the project. *)
val ace_editor_src : unit -> Eliom_content_core.Xml.uri

(** [ace_alias_src ()] returns the alias to the [ace editor] in the
    project, avoiding common name clash with identifiers generated
    by js_of_ocaml (!). *)
val ace_alias_src : unit -> Eliom_content_core.Xml.uri

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

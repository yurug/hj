(* -*- tuareg -*- *)

(** Configuration parameters. *)

(** [ressource_root] is the place where the state of the system is stored. *)
val ressource_root : string

(** [autotest_enabled ()] returns [true] if the server is in
    autotesting mode. This mode is enabled if the environment 
    variable [AUTOTEST] is set. *)
val autotest_enabled : unit -> bool

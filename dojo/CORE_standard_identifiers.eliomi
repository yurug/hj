(* -*- tuareg -*- *)

open CORE_identifier

(** Standard identifiers. *)

val root : bool -> path -> path

val tests_path : path

val users_path : path

val exercises_path : path

val questions_path : path

val system_path : path

val std_paths : path list

val fresh : path -> string -> identifier

val assigner : identifier

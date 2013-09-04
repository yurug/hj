(* -*- tuareg -*- *)

(** Extension to Lwt_process. *)

open Lwt_process

val ( @@ ) : string -> string -> string

type command

val ( !% ) : string -> command

val success : command -> bool Lwt.t

val grep : command -> string -> string Lwt_stream.t

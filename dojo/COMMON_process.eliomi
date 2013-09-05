(* -*- tuareg -*- *)

(** Extension to Lwt_process. *)

open Lwt_process

val ( @@ ) : string -> string -> string

type command

val strace : (Lwt_process.command -> 'a) -> command -> 'a

val ( !% ) : string -> command

val success :
  ?lraise:([> `SystemError of string ] -> 'e)
  -> command
  -> bool Lwt.t

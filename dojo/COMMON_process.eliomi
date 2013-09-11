(* -*- tuareg -*- *)

(** Extension to Lwt_process. *)

open Lwt_process

val ( @@ ) : string -> string -> string

type command

val strace : (Lwt_process.command -> 'a) -> command -> 'a

val ( !% ) : string -> command

val pread :
  ?lraise:([> `SystemError of string ] -> 'e Lwt.t)
  -> command
  -> string Lwt.t

val pread_lines :
  ?lraise:([> `SystemError of string ] -> 'e Lwt.t)
  -> command
  -> string Lwt_stream.t Lwt.t

val blind_exec: command -> Unix.process_status Lwt.t

val success :
  ?lraise:([> `SystemError of string ] -> 'e Lwt.t)
  -> command
  -> bool Lwt.t

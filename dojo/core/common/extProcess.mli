(* -*- tuareg -*- *)

(** Extension to Lwt_process. *)

open Lwt_process

val ( @@ ) : string -> string -> string

type command

val strace : (Lwt_process.command -> 'a Lwt.t) -> command -> 'a Lwt.t

val strace' : (Lwt_process.command -> 'a) -> command -> 'a * (unit -> unit)

val ( !% ) : string -> command

val pread :
  ?lraise:([> `SystemError of string ] -> 'e Lwt.t)
  -> command
  -> string Lwt.t

val pread_lines :
  ?lraise:([> `SystemError of string ] -> 'e Lwt.t)
  -> command
  -> (string Lwt_stream.t * (unit -> unit)) Lwt.t

val blind_exec: command -> Unix.process_status Lwt.t

val exec: ?timeout:float -> command -> Lwt_process.process_full * (unit -> unit)

val success :
  ?lraise:([> `SystemError of string ] -> 'e Lwt.t)
  -> command
  -> bool Lwt.t

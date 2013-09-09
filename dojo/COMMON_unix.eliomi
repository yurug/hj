(* -*- tuareg -*- *)

(** Wrap most common UNIX commands. *)

type ('a, 'b) raiser = ([> `SystemError of string ] as 'a) -> 'b Lwt.t

(** [mkdir path]. *)
val mkdir : string -> ('a, 'b) raiser -> unit Lwt.t

(** [rmdir rdir ~content:f]. *)
val rmdir : string -> ?content:bool -> ('a, 'b) raiser -> unit Lwt.t

(** [grep cmd regexp] returns the stream of lines produces by [cmd]
    that match [regexp]. *)
val grep :
  COMMON_process.command -> string -> ('a, 'b) raiser
  -> string Lwt_stream.t Lwt.t

(** [echo content fname] saves the [content] in [fname]. *)
val echo : string -> string -> ('a, 'b) raiser -> unit Lwt.t

(** [cat fname] returns the content of [fname] (if it fits in a string). *)
val cat : string -> ('a, 'b) raiser -> string Lwt.t

(** [split cmd delim] splits each line produced by [cmd] using
    regexp [delim]. *)
val split :
  COMMON_process.command -> string -> ('a, 'b) raiser
  -> string list Lwt_stream.t Lwt.t

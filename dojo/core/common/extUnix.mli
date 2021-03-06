(* -*- tuareg -*- *)

(** Wrap common UNIX commands. *)
open ExtProcess

(** [quote s] *)
val quote : string -> string

type ('a, 'b) raiser = ([> `SystemError of string ] as 'a) -> 'b Lwt.t

(** [mkdir path]. *)
val mkdir : string -> ('a, 'b) raiser -> unit Lwt.t

(** [rmdir rdir ~content:f]. *)
val rmdir : string -> ?content:bool -> ('a, 'b) raiser -> unit Lwt.t

(** [ls dir]. *)
val ls : ?relative:bool -> string -> ('a, 'b) raiser -> string list Lwt.t

(** [cp src dest]. *)
val cp : string -> string -> ('a, 'b) raiser -> unit Lwt.t

(** [tar cvfz dest files]. *)
val tar_create : string -> string list -> ('a, 'b) raiser -> unit Lwt.t

(** [tar xvfz tarfile dest]. *)
val tar_expand : string -> string -> ('a, 'b) raiser -> unit Lwt.t

(** [grep cmd regexp] returns the stream of lines produces by [cmd]
    that match [regexp]. *)
val grep : command -> string -> ('a, 'b) raiser -> string Lwt_stream.t Lwt.t

(** [echo content fname] saves the [content] in [fname]. *)
val echo : string -> string -> ('a, 'b) raiser -> unit Lwt.t

(** [append content fname] appends the [content] at the end of [fname]. *)
val append : string -> string -> ('a, 'b) raiser -> unit Lwt.t

(** [cat fname] returns the content of [fname] (if it fits in a string). *)
val cat : string -> ('a, 'b) raiser -> string Lwt.t

(** [nothing] does nothing. *)
val nothing : ('a, 'b) raiser -> unit Lwt.t

(** [split cmd delim] splits each line produced by [cmd] using
    regexp [delim]. *)
val split
  : ExtProcess.command -> string -> ('a, 'b) raiser
  -> string list Lwt_stream.t Lwt.t

(** [now ()] returns a string representation of the current time. *)
val now : ('a, 'b) raiser -> string Lwt.t

(** [ssh ?timeout username key addr port cmd observer] runs a process
    which executes a remote command through a secured connection.
    This function returns a function to cancel this command at any time. *)
val ssh :
  ?timeout:float
  -> string -> string
  -> string -> int
  -> string
  -> (Lwt_process.process_full -> unit Lwt.t)
  -> ('a, 'b) raiser
  -> (unit -> unit) Lwt.t

(** [scp ?timeout username key addr port src dst observer] runs a process
    which copies a file through a secured connection.
    This function returns a function to cancel this command at any time. *)
val scp :
  ?timeout:float
  -> string -> string
  -> string -> int
  -> string list
  -> (Lwt_process.process_full -> unit Lwt.t)
  -> ('a, 'b) raiser
  -> (unit -> unit) Lwt.t

val pdflatex
  : string -> string
  -> ([> `SystemError of string ], 'b) raiser -> bool Lwt.t

val mail:
  mailer:string ->
  domain:string ->
  target_email:string ->
  target_name:string ->
  subject:string ->
  message:string -> unit

module ExtFilename : sig

  (** [temp_filename tmpdir prefix suffix] returns a fresh name for a
      file. Only guarantee that the file does not exist when called. *)
  val temp_filename : ?temp_dir:string -> string -> string -> string Lwt.t

end


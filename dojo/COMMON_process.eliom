(* -*- tuareg -*- *)

(** Abstraction on top of Lwt_process. *)

open Str
open Lwt
open Lwt_process
open COMMON_log
open COMMON_pervasives

let default_timeout = 700.

let ( @@ ) at c = Printf.sprintf "(cd %s && %s)" at c

type command = Lwt_process.command * string

let ( !% ) s = (shell s, s)

let strace f (cmd, s) =
  log [Strace] s;
  f cmd

let string_of_process_status = function
  | Unix.WEXITED   d -> Printf.sprintf "Exited %d" d
  | Unix.WSIGNALED d -> Printf.sprintf "Signaled %d" d
  | Unix.WSTOPPED  d -> Printf.sprintf "Stopped %d" d

let pread ?(lraise=small_jump) c =
  try_lwt
    strace (pread ~timeout:default_timeout ~stdin:`Dev_null ~stderr:`Dev_null) c
  with _ ->
    (lraise @* (`SystemError "pread"))
    @| (fun () -> return "(null)")

let pread_lines ?(lraise=small_jump) c =
  try_lwt
    return (strace (
      pread_lines ~timeout:default_timeout ~stdin:`Dev_null ~stderr:`Dev_null) c
    )
  with _ ->
    (lraise @* (`SystemError "pread_lines"))
    @| (fun () -> return (Lwt_stream.of_list []))

let blind_exec c =
  strace (
    exec
      ~timeout:default_timeout
      ~stdin:`Dev_null
      ~stdout:`Dev_null
      ~stderr:`Dev_null
  ) c

let success ?(lraise=small_jump) c =
  blind_exec c
  >>= function
  | Unix.WEXITED 0 ->
    return_true
  | s ->
    let string_of_process_command c =
      fst c ^ " " ^ String.concat " " (Array.to_list (snd c))
    in
    let s =
      string_of_process_command (fst c) ^ ":" ^ string_of_process_status s
    in
    (lraise @* (`SystemError s))
    @| (fun () -> return_false)

let exec ?timeout c =
  strace (open_process_full ?timeout) c

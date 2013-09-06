(* -*- tuareg -*- *)

(** Extension to Lwt_process. *)

open Str
open Lwt
open Lwt_process
open COMMON_log
open COMMON_pervasives

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

let success ?(lraise=small_jump) c =
  strace (exec ~stdin:`Dev_null ~stdout:`Dev_null ~stderr:`Dev_null) c
  >>= function
  | Unix.WEXITED 0 ->
    return_true
  | s ->
    let s = string_of_process_status s in
    log [Strace] (Printf.sprintf "   Status: %s\n" s);
    (lraise @* (`SystemError s))
    @| (fun () ->
      wrap (fun () -> log [Strace] (Printf.sprintf "   Escaping!"))
      >> return_false)

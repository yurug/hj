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
    (lraise @* (`SystemError (string_of_process_status s)))
    @| return_false

let grep c pattern =
  let s = strace (pread_lines ~stdin:`Dev_null ~stderr:`Dev_null) c in
  Lwt_stream.filter_map (fun s ->
    if string_match (regexp pattern) s 0 then
      try
        Some (matched_group 1 s)
      with Not_found -> None
    else
      None
  ) s

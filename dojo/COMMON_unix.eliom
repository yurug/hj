(* -*- tuareg -*- *)

open Lwt
open Lwt_io
open Lwt_stream
open Lwt_process

open COMMON_log
open COMMON_pervasives
open COMMON_process
open Str

type ('a, 'b) raiser = ([> `SystemError of string ] as 'a) -> 'b Lwt.t

let handle_unix_error f default lraise =
  try_lwt
    f ()
  with Unix.Unix_error (e, _, _) ->
    (lraise @* (`SystemError (Unix.error_message e)))
    @| (fun () -> return default)

let mkdir ps =
  handle_unix_error (fun () ->
    Lwt_unix.mkdir ps 0o700
  ) ()

let ls ps =
  handle_unix_error Lwt_unix.(fun () ->
    lwt dh = opendir ps in
    let rec all ls =
      try_lwt
        lwt file = readdir dh in
        all (Filename.concat ps file :: ls)
      with End_of_file -> return ls
    in
    lwt y = all [] in
    closedir dh >>
    return y
  ) []


let rmdir ps ?(content=false) lraise =
  let cmd = Printf.sprintf (if content then "rm -fr %s" else "rm -r %s") ps in
  success ~lraise (!% cmd)
  >> return ()

let cp src dst lraise =
  let cmd = Printf.sprintf "cp %s %s" src dst in
  success ~lraise (!% cmd)
  >> return ()

let read c =
  handle_unix_error (fun () -> return (
    strace (Lwt_process.pread_lines ~stdin:`Dev_null ~stderr:`Dev_null) c
  )) (Lwt_stream.of_list [])

let grep c pattern =
  read c >-> (fun s ->
  lreturn (
    Lwt_stream.filter_map (fun s ->
      if string_match (regexp pattern) s 0 then
        try
          Some (matched_group 1 s)
        with Not_found -> None
      else
        None
    ) s
  ))

let echo c f =
  handle_unix_error (fun () ->
    with_file
      ~flags:[Unix.O_CREAT; Unix.O_WRONLY; Unix.O_TRUNC ]
      ~mode:output f (fun oc ->
      write oc c
    )
  ) ()

let append c f =
  handle_unix_error (fun () ->
    with_file
      ~flags:[Unix.O_APPEND; Unix.O_WRONLY ]
      ~mode:output f (fun oc ->
      write oc c
    )
  ) ()

let cat f =
  handle_unix_error (fun () ->
    let b = Buffer.create 13 in
    Lwt_stream.iter
      (Buffer.add_char b)
      (Lwt_io.chars_of_file f)
    >> return (Buffer.contents b)
  ) "(empty)"

let split c delim =
  handle_unix_error (fun () ->
    let s =
      strace (Lwt_process.pread_lines ~stdin:`Dev_null ~stderr:`Dev_null) c
    in
    return (
      Lwt_stream.filter_map (fun s ->
        try
          Some (Str.split (regexp delim) s)
        with _ -> None
      ) s
    )
  ) (Lwt_stream.of_list [])

let now lraise =
  (read (!% "date") >-> fun s _ -> Lwt_stream.next s) lraise

let ssh ?timeout username private_key addr port cmd observer =
  let os = "-o 'StrictHostKeyChecking=no' -o 'UserKnownHostsFile=/dev/null'" in
  handle_unix_error (fun () ->
    let p = exec ?timeout (!% (
      Printf.sprintf
        "ssh %s@%s %s -p %d -i %s '(%s)'"
        username addr os port private_key cmd))
    in
    observer p >> return (fun () -> p#terminate)
  ) (fun () -> ())

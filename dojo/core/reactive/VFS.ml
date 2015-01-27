(* -*- tuareg -*- *)

open Lwt
open Lwt_stream
open Lwt_io

open ExtPervasives
open ExtProcess
open ExtUnix
open Log

open Identifier

(** The file system is rooted there: *)
let set_root, get_root = oref (fun () ->
  Error.fatal ReactiveErrors.MissingConfiguredRoot
)

let root_path () = path_of_string (get_root ())
let root relative p = if relative then concat (root_path ()) p else p

let relativize p =
  suffix (root_path ()) p

let relativize_identifier p =
  identifier_of_path (relativize (path_of_identifier p))

(** The file system is the exclusive property of the current user. *)
let _ = Unix.umask 0o077

(** {1 Functional part.} *)

type filename = Identifier.t

type stored_version = {
    number    : string;
    author    : string;
    date      : string;
    timestamp : Int64.t;
    path      : path;
  }

type version =
  | Latest of path * (unit -> version Lwt.t)
  | Stored of stored_version

let git_commit who where what message lraise =
  success ~lraise
    (!% (where
         @@ (Printf.sprintf
               (** We use --allow-empty because we want to record the
                   commit action, even if it has no effect on the
                   file system. *)
               ("git commit --allow-empty --author='%s' -m %s %s")
               who (Filename.quote message)
               (String.concat " " (List.map Filename.quote what))
         )
     ))

let git_add where what lraise =
  success ~lraise
    (!% (where
         @@ (Printf.sprintf
               ("git add %s")
               (String.concat " " (List.map Filename.quote what))
         )
     ))

let git_versions where what lraise =
  lwt log =
    split (!% (where
             @@ (Printf.sprintf
                   ("git log --pretty=format:\"%%H|%%an|%%ct|%%cd\" %s")
                   (Filename.quote what))))
      "|"
      lraise
  in
  let make_version l =
    let get = List.nth l in
    try
      Some (Stored {
        number    = get 0;
        author    = get 1;
        date      = get 3;
        timestamp = Int64.of_string (get 2);
        path      = path_of_string (Filename.concat where what);
      })
    with _ ->
      (* FIXME: Log this. *)
      None
  in
  Lwt_stream.to_list (Lwt_stream.filter_map make_version log)

let git_show hash what lraise =
  let where = Filename.dirname what in
  let fname = Filename.basename what in
  pread (!% (where @@ Printf.sprintf "git show %s:%s" hash fname))

let git_toplevel where lraise =
  pread_lines (!% (where @@ "git rev-parse --show-toplevel")) ~lraise
  >>= (fun (s, stop) -> stop (); last_new s)

let on_path f p =
  let p = root true p in
  let ps = string_of_path p in
  let fname = Filename.basename ps in
  let where = Filename.dirname ps in
  f p ps fname where

let create_dir_if_absent relative dir lraise =
  let dir = string_of_path (root relative dir) in
  blind_exec (!% (get_root () @@ (Printf.sprintf "test -d %s" dir)))
  >>= function
    | Unix.WEXITED 0 -> return ()
    | _ -> mkdir dir lraise

let init_root ipath =
  set_root ipath;
  let under_git _ =
    blind_exec (!% (get_root () @@ "test -d .git"))
  in
  let git_init _ =
    blind_exec (!% (get_root () @@ "git init"))
  in
  ltry (
    !>> create_dir_if_absent false (root_path ())
    >>> under_git
    >-> (function
      | (Unix.WEXITED 0) as x -> lreturn x
      | _ -> git_init
    )
    >>> lreturn ()
  )

let create who = on_path (fun p ps _ _ ->
  let check_if_filename_already_exists lraise =
    try_lwt
      ignore (Sys.is_directory ps);
      lraise (`AlreadyExists p)
    with Sys_error _ -> return ()
   in
  let git_init lraise =
    success ~lraise (!% (ps @@ "git init"))
  in
  ltry (
    !>> check_if_filename_already_exists
    >>> mkdir ps
    >>> git_init
    >>> lreturn ()
  ))

let create_tmp who =
  lwt dname =
    ExtFilename.temp_filename ~temp_dir:(get_root ()) "tmp" ""
  in
  let path = path_of_string dname in
  ltry (
    !!> (create who @* path)
    >>> lreturn path
  )

let delete who = on_path (fun p ps _ _ ->
  let check_if_directory_exists lraise =
    try_lwt
      return (Sys.is_directory ps)
    with Sys_error _ ->
      lraise (`DirectoryDoesNotExist p)
   in
  ltry (
    !>> check_if_directory_exists
    >>> rmdir ~content:true ps
    >>> lreturn ()
  ))

let versions = on_path (fun _ _ fname where -> ltry (
  git_versions where fname
))

let rec get w = function
  | Latest (_, r) -> r () >>= get w
  | Stored s -> return (w s)

let date = get (fun s -> s.date)

let timestamp = get (fun s -> s.timestamp)

let author = get (fun s -> s.author)

let number = get (fun s -> s.number)

let path = get (fun s -> s.path)

let latest = on_path (fun p _ fname where -> ltry (fun lraise ->
  return (Latest (p, fun () ->
    lwt vs = git_versions where fname lraise
    in return (List.hd vs))))
)

let read v = ltry (fun lraise ->
  match v with
    | Latest (p, _) ->
      Printf.eprintf "cat %s\n%!" (string_of_path p);
      cat (string_of_path p) lraise
    | Stored v ->
      Printf.eprintf "git show %s %s\n%!" (string_of_path v.path) v.number;
      git_show v.number (string_of_path v.path) lraise
)

let version_from_number path n =
  versions path >>>= fun vs ->
  try_lwt
    Lwt_list.find_s (fun v ->
      lwt nv = number v in
      return (nv = n)
    ) vs >>= fun x ->
    return (`OK x)
  with Not_found ->
    return (`KO `NoSuchVersion)

let version_from_number path n =
  versions path >>>= fun vs ->
  try_lwt
    Lwt_list.find_s (fun v ->
      lwt nv = number v in
      return (nv = n)
    ) vs >>= fun x ->
    return (`OK x)
  with Not_found ->
    return (`KO `NoSuchVersion)

let onfile f who =
  on_path (fun p ps fname where c on_finished -> ltry (
  !>> f c ps
  >>> git_add where [fname]
  >>> git_commit who where [fname] ps
  >>> lreturn ()
) >>= function
  | `OK () -> on_finished () >> return (`OK ())
  | `KO e -> return (`KO e)
)

let save   = onfile echo
let append = onfile append

let commit p ps = onfile (fun _ _ -> ExtUnix.nothing) p ps ()

let owner = on_path (fun _ where _ _ -> ltry (
  git_toplevel where
  >-> fun s -> lreturn (path_of_string s)
))

let real_path path =
  string_of_path (root true path)

let exists path =
  Sys.file_exists (real_path path)

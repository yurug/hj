(* -*- tuareg -*- *)

open Lwt
open Lwt_stream
open Lwt_io

open CORE_config
open CORE_identifier
open CORE_standard_identifiers
open CORE_error_messages
open COMMON_pervasives
open COMMON_process
open COMMON_log
open COMMON_unix

(** {1 Functional part.} *)

type filename = CORE_identifier.t

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
      Ocsigen_messages.errlog ("Fail");
      None
  in
  Lwt_stream.to_list (Lwt_stream.filter_map make_version log)

let git_show hash what lraise =
  let where = Filename.dirname what in
  let fname = Filename.basename what in
  pread (!% (where @@ Printf.sprintf "git show %s:./%s" hash fname))

let git_toplevel where lraise =
  pread_lines (!% (where @@ "git rev-parse --show-toplevel")) ~lraise
  >>= last_new

let on_path f ?(relative = true) p =
  let p = root relative p in
  let ps = string_of_path p in
  let fname = Filename.basename ps in
  let where = Filename.dirname ps in
  f p ps fname where

let init_root () =
  let create_dir_if_absent relative dir lraise =
    let dir = string_of_path (root relative dir) in
    blind_exec (!% (ressource_root @@ (Printf.sprintf "test -d %s" dir)))
    >>= function
      | Unix.WEXITED 0 -> return ()
      | _ -> mkdir dir lraise
  in
  let under_git _ =
    blind_exec (!% (ressource_root @@ "test -d .git"))
  in
  let git_init _ =
    blind_exec (!% (ressource_root @@ "git init"))
  in
  ltry (
    !>> create_dir_if_absent false (path_of_string ressource_root)
    >>> under_git
    >-> (function
      | (Unix.WEXITED 0) as x -> lreturn x
      | _ -> git_init
    )
    >>> create_dir_if_absent true tests_path
    >>> create_dir_if_absent true users_path
    >>> create_dir_if_absent true exercises_path
    >>> create_dir_if_absent true system_path
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
    ExtFilename.temp_filename ~temp_dir:ressource_root "tmp" ""
  in
  let path = path_of_string dname in
  ltry (
    !!> (create who ~relative:false @* path)
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
      cat (string_of_path p) lraise
    | Stored v ->
      git_show v.number (string_of_path v.path) lraise
)

let save who = on_path (fun p ps fname where c -> ltry (
  !>> echo c ps
  >>> git_add where [fname]
  >>> git_commit who where [fname] (I18N.String.saving ps)
  >>> lreturn ()
))

let owner = on_path (fun _ where _ _ -> ltry (
  git_toplevel where
  >-> fun s -> lreturn (path_of_string s)
))

(** {1 Unit testing.} *)

type inconsistency =
  | NoRootRepository
  | Untracked of filename list
  | BrokenOperation of broken_operation_description

and broken_operation_description = {
  operation : [`Create | `Delete | `Save | `Versions | `Read | `Owner ];
  reason    : string;
}

type consistency_level =
  | Consistent
  | Inconsistent of inconsistency

let string_of_operation = function
  | `Create   -> "vfs.create"
  | `Delete   -> "vfs.delete"
  | `Save     -> "vfs.save"
  | `Versions -> "vfs.versions"
  | `Read     -> "vfs.read"
  | `Owner    -> "vfs.owner"

let string_of_inconsistency = function
  | NoRootRepository ->
    I18N.String.there_is_no_repository_at_ressource_root
  | Untracked fs ->
    I18N.String.the_following_files_are_untracked (
      List.map string_of_identifier fs
    )
  | BrokenOperation { operation; reason } ->
    I18N.String.the_following_operation_is_broken
      (string_of_operation operation)
      reason

let string_of_consistency_level = function
  | Consistent -> I18N.String.the_filesystem_is_consistent
  | Inconsistent c -> string_of_inconsistency c

let ensure_consistency test icp =
  lwt_if test
    (return Consistent)
    (lwt ic = icp in return (Inconsistent ic))

let there_is_repository_at_resssource_root () =
  ensure_consistency
    (success (!% (ressource_root @@ "test -d .git")))
    (return NoRootRepository)

let there_is_no_untracked_files () =
  lwt untracked_files = grep
    (!% (ressource_root @@ "git status --porcelain"))
    ("\\?\\? \\(.*\\)")
    (warn_only "core_vfs.there_is_no_untracked_files.grep failed.")
  in
  ensure_consistency
    (is_empty untracked_files)
    (lwt fs = to_list untracked_files in
     let fs = List.map identifier_of_string fs in
     return (Untracked fs))

let who = "system.test.vfs <here@hackojo.org>"

let operation_works operation scenario =
  scenario >>= function
    | `OK _ -> return Consistent
    | `KO e -> return (Inconsistent (BrokenOperation {
      operation = operation;
      reason    = string_of_error e
    }))

let vfs_create_works () =
  operation_works `Create (
    create_tmp who
  )

let in_tmp_dir f =
  !>>> create_tmp who
  >>>= (fun x ->
    f x
    >>>= delete who ~relative:false @* x)

let vfs_delete_works () =
  operation_works `Delete (
    !>>> create_tmp who
    >>>= fun x -> delete who ~relative:false x
  )

let vfs_save_works () =
  operation_works `Save (
    in_tmp_dir (fun x ->
      save who ~relative:false (concat x (make [label "test"])) "Test"
    )
  )

let vfs_versions_works () =
  operation_works `Versions (
    in_tmp_dir (fun x ->
      let fname = concat x (make [label "test"]) in
      !>>> (save who ~relative:false fname "Test1")
      >>>= save who ~relative:false fname @* "Test2"
      >>>= (fun () ->
        versions ~relative:false fname >>>= fun vs ->
        if List.length vs <> 2 then
          return (`KO (`SystemError "git log"))
        else
          return (`OK ())
      )
    )
  )

let vfs_read_works () =
  operation_works `Read (
    in_tmp_dir (fun x ->
      let fname = concat x (make [label "test"]) in
      !>>> (save who ~relative:false fname "Test1")
      >>>= save who ~relative:false fname @* "Test2"
      >>>= (fun () ->
        versions ~relative:false fname >>>= function
          | [ _; v ] -> (
            read v >>= function
              | `OK c ->
                if c <> "Test1" then
                  return (`KO (`SystemError "invalid file content"))
                else
                  return (`OK ())
              | `KO s -> return (`KO s)
          )
          | _ -> return (`KO (`SystemError "read because of version"))
      )
    )
  )

let vfs_owner_works () =
  operation_works `Owner (
    in_tmp_dir (fun x ->
      owner ~relative:false x
      >>>= (fun y ->
        if x <> y then
          return (`KO (`SystemError (Printf.sprintf "|%s| <> |%s|"
                                       (string_of_path x)
                                       (string_of_path y))))
        else return (`OK ())
      )
    )
  )

let check () =
  continue_while_is Consistent [
    there_is_repository_at_resssource_root;
    there_is_no_untracked_files;
    vfs_create_works;
    vfs_delete_works;
    vfs_save_works;
    vfs_versions_works;
    vfs_read_works;
    vfs_owner_works;
  ]

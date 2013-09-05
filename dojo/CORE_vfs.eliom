(* -*- tuareg -*- *)

open Lwt
open Lwt_stream
open CORE_identifier
open COMMON_pervasives
open COMMON_process
open COMMON_log
open COMMON_unix

(** {1 Functional part.} *)

type filename = CORE_identifier.t

let root relative p =
  if relative then
    path_of_string CORE_config.ressource_root @ p
  else
    p

let git_commit who where what message lraise =
  success ~lraise
    (!% (where
         @@ (Printf.sprintf
               ("git commit --author='%s' -m %s %s")
               who (Filename.quote message)
               (String.concat " " (List.map Filename.quote what))
         )
     ))

let on_path f ?(relative = true) p =
  let p = root relative p in
  let ps = string_of_path p in
  f p ps

let create who = on_path (fun p ps ->
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
    ExtFilename.temp_filename ~temp_dir:CORE_config.ressource_root "tmp" ""
  in
  let path = path_of_string dname in
  ltry (
    !>> abs_error (create who ~relative:false path)
    >>> lreturn path
  )

let delete who = on_path (fun p ps ->
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

let save who = on_path (fun p ps c ->
  lreturn ()
)

(** {1 Unit testing.} *)

type inconsistency =
  | NoRootRepository
  | Untracked of filename list
  | BrokenOperation of broken_operation_description

and broken_operation_description = {
  operation : [`Create | `Delete ];
  reason    : string;
}

type consistency_level =
  | Consistent
  | Inconsistent of inconsistency

let string_of_operation = function
  | `Create -> "vfs.create"
  | `Delete -> "vfs.delete"

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
    (success (!% (CORE_config.ressource_root @@ "test -d .git")))
    (return NoRootRepository)

let there_is_no_untracked_files () =
  let untracked_files = grep
    (!% (CORE_config.ressource_root @@ "git status --porcelain"))
    ("\\?\\? \\(.*\\)")
  in
  ensure_consistency
    (is_empty untracked_files)
    (lwt fs = to_list untracked_files in
     let fs = List.map identifier_of_string fs in
     return (Untracked fs))

let who = "system.test.vfs <here@hackojo.org>"

let string_of_error = function
  | `SystemError e ->
    "System: " ^ e
  | `AlreadyExists p ->
    I18N.String.the_following_file_already_exists (string_of_path p)
  | `DirectoryDoesNotExist p ->
    I18N.String.the_following_directory_does_not_exist (string_of_path p)

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

let ( >=> ) e f lraise =
  lwt x = e lraise in
  f x lraise

let vfs_delete_works () =
  operation_works `Create (ltry (
    !>> abs_error (create_tmp who)
    >=> fun x -> abs_error (delete who ~relative:false x)
  ))

let check () =
  continue_while_is Consistent [
    there_is_repository_at_resssource_root;
    there_is_no_untracked_files;
    vfs_create_works;
    vfs_delete_works;
  ]

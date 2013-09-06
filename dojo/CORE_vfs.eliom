(* -*- tuareg -*- *)

open Lwt
open Lwt_stream
open Lwt_io

open CORE_identifier
open COMMON_pervasives
open COMMON_process
open COMMON_log
open COMMON_unix

(** {1 Functional part.} *)

type filename = CORE_identifier.t

type version = {
  number : string;
  author : string;
  date   : string;
  path   : path;
}

let root relative p =
  if relative then
    concat (path_of_string CORE_config.ressource_root) p
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
                   ("git log --pretty=format:\"%%H %%an %%cd\" %s")
                   (Filename.quote what))))
      " "
      lraise
  in
  let make_version l =
    let get = List.nth l in
    try
      Some {
        number = get 0;
        author = get 1;
        date   = get 2;
        path   = CORE_identifier.path_of_string (Filename.concat where what)
      }
    with _ -> None
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
    ExtFilename.temp_filename ~temp_dir:CORE_config.ressource_root "tmp" ""
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

let date v = v.date

let author v = v.author

let number v = v.number

let read v = ltry (
  git_show v.number (CORE_identifier.string_of_path v.path)
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
    (success (!% (CORE_config.ressource_root @@ "test -d .git")))
    (return NoRootRepository)

let there_is_no_untracked_files () =
  lwt untracked_files = grep
    (!% (CORE_config.ressource_root @@ "git status --porcelain"))
    ("\\?\\? \\(.*\\)")
    (warn_only "core_vfs.there_is_no_untracked_files.grep failed.")
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

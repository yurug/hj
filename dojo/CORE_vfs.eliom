(* -*- tuareg -*- *)

open Lwt
open Lwt_stream
open CORE_identifier
open COMMON_pervasives
open COMMON_process

type filename = string

type inconsistency =
  | NoRootRepository
  | Untracked of filename list

type consistency_level =
  | Consistent
  | Inconsistent of inconsistency

let string_of_inconsistency = function
  | NoRootRepository ->
    I18N.String.there_is_no_repository_at_ressource_root
  | Untracked fs ->
    I18N.String.the_following_files_are_untracked fs

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
    (lwt fs = to_list untracked_files in return (Untracked fs))

let check () =
  continue_while_is Consistent [
    there_is_repository_at_resssource_root;
    there_is_no_untracked_files
  ]

(* -*- tuareg -*- *)

open Lwt
open CORE_identifier

type file = CORE_identifier.t

type inconsistency =
  | NoRootRepository
  | Untracked of file

type consistency_level =
  | Consistent
  | Inconsistent of inconsistency

let string_of_inconsistency = function
  | NoRootRepository ->
    I18N.String.there_is_no_repository_at_ressource_root
  | Untracked f ->
    I18N.String.the_following_file_is_untracked (string_of_identifier f)

let string_of_consistency_level = function
  | Consistent -> I18N.String.the_filesystem_is_consistent
  | Inconsistent c -> string_of_inconsistency c

let check () =
  return Consistent

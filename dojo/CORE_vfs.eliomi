(* -*- tuareg -*- *)

(** A versioned hierarchical file system. *)

(** This module implements a versioned hierarchical filesystem rooted
    at [CORE_config.ressource_root].

    It is hierarchical in the usual way: one can address a stored file
    using a [CORE_identifier.t] which is a singularized path. But, it
    is also hierarchical in terms of history. Indeed, the history of
    the file system is made of the independent histories of local
    sub-filesystems which also are singularized.

    This module is based on the GIT control version system,
    @see <http://git-scm.com/> the official GIT website for general
    information about this tool.

*)

(** A file system contains files. *)
type filename = CORE_identifier.t

(** The VFS might be incoherent for one of the following reasons: *)
type inconsistency =
  (** There is no repository rooted at [CORE_config.ressource_root]. *)
  | NoRootRepository
  (** There exists a file that is not tracked by any repository. *)
  | Untracked of filename list

(** File system consistency.*)
type consistency_level =
  | Consistent
  | Inconsistent of inconsistency

(** [string_of_consistency_level c] is human readable description
    of the consistency level. *)
val string_of_consistency_level : consistency_level -> string

(** [check ()] if the file system rooted at
    [CORE_config.ressource_root] is in a coherent state.  *)
val check: unit -> consistency_level Lwt.t

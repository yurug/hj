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
open Identifier

(** {1 Functional parts.} *)

(** A file system contains files. *)
type filename = Identifier.t

(** [init_root ()] initializes the root of the file system if required. *)
val init_root : unit
  -> [ `OK of unit
     | `KO of [ `SystemError of string ]
     ] Lwt.t

(** [create who path] initializes a subvfs at [path], authored by
    [who].  The [path] must not exist in the root vfs. All the needed
    directories are created on-the-fly if they do not exist.
*)
val create:
  string -> ?relative:bool -> path
  -> [ `OK of unit
     | `KO of
         (** The path is already taken. *)
         [> `AlreadyExists of path
         (** Something went wrong at the system level.
             (It may be git-related or os-related.) *)
         | `SystemError of string
         ]
     ] Lwt.t

(** [delete who path] deletes a subvfs at [path].  The [path] must
    exist in the root vfs. *)
val delete: string -> ?relative:bool -> path
  -> [ `OK of unit
     | `KO of
         (** The path is invalid. *)
         [> `DirectoryDoesNotExist of path
         (** Something went wrong at the system level.
             (It may be git-related or os-related.) *)
         | `SystemError of string
         ]
     ] Lwt.t

(** [save who path content] stores the string [content] in the file
    at [path]. If there is no file at [path], it is created. *)
val save : string -> ?relative:bool -> path -> string
  -> [ `OK of unit
     | `KO of
         (** Something went wrong at the system level.
             (It may be git-related or os-related.) *)
         [> `SystemError of string
         ]
     ] Lwt.t

(** [append who path content] appends the string [content] in the end
    of the file located at [path]. If there is no file at [path], it
    is created. *)
val append : string -> ?relative:bool -> path -> string
  -> [ `OK of unit
     | `KO of
         (** Something went wrong at the system level.
             (It may be git-related or os-related.) *)
         [> `SystemError of string
         ]
     ] Lwt.t

(** a [version] is timestamp file. *)
type version

(** [versions path] returns the list of versions of the file
    located at [path]. *)
val versions : ?relative:bool -> path
  -> [ `OK of version list
     | `KO of
         (** Something went wrong at the system level.
             (It may be git-related or os-related.) *)
         [> `SystemError of string
         ]
     ] Lwt.t

(** [latest id] is the latest version of the file
    located at [path]. *)
val latest : ?relative:bool -> path
  -> [ `OK of version
     | `KO of
         (** Something went wrong at the system level.
             (It may be git-related or os-related.) *)
         [> `SystemError of string
         ]
     ] Lwt.t

(** [number version] returns a string identifier for the
    [version]. *)
val number : version -> string Lwt.t

(** [author version] returns the author of a [version]. *)
val author : version -> string Lwt.t

(** [date version] returns the date of a [version]. *)
val date : version -> string Lwt.t

(** [timestamp version] returns the UNIX timestamp of a [version]. *)
val timestamp : version -> Int64.t Lwt.t

(** [read version] returns the content of a version. *)
val read : version
  -> [ `OK of string
     | `KO of (** Something went wrong at the system level.
             (It may be git-related or os-related.) *)
         [> `SystemError of string
         ]
     ] Lwt.t

(** [owner path] returns the path of the nearest englobing [subvfs]
    from [path]. *)
val owner : ?relative:bool -> Identifier.path
  -> [ `OK of Identifier.path
     | `KO of (** Something went wrong at the system level.
                  (It may be git-related or os-related.) *)
         [> `SystemError of string
         ]
     ] Lwt.t

(** {1 Unit tests.} *)

(** The VFS might be incoherent for one of the following reasons: *)
type inconsistency =
  (** There is no repository rooted at [CORE_config.ressource_root]. *)
  | NoRootRepository
  (** There exists a file or a directory that is not tracked by any
      repository. *)
  | Untracked of filename list
  (** One of the basic operations is broken. *)
  | BrokenOperation of broken_operation_description

and broken_operation_description = {
  operation : [`Create | `Delete | `Save | `Versions | `Read | `Owner ];
  reason    : [
  | `Inconsistency         of inconsistency
  | `AlreadyExists         of path
  | `SystemError           of string
  | `DirectoryDoesNotExist of path
  ]
}

(** File system consistency.*)
type consistency_level =
  | Consistent
  | Inconsistent of inconsistency

(** [check ()] if the file system rooted at
    [CORE_config.ressource_root] is in a coherent state.  *)
val check: unit -> consistency_level Lwt.t

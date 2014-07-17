(* -*- tuareg -*- *)

(** A versioned hierarchical file system. *)

(** This module implements a versioned hierarchical filesystem.

    It is hierarchical in the usual way: one can address a stored file
    using a [Identifier.t] which is a singularized path. But, it
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

(** [init_root path] initializes the root of the file system at [path]
    if required. *)
val init_root : string
  -> [ `OK of unit
     | `KO of [ `SystemError of string ]
     ] Lwt.t

(** [create who path] initializes a subvfs at [path], authored by
    [who].  The [path] must not exist in the root vfs. All the needed
    directories are created on-the-fly if they do not exist.
*)
val create: string -> path
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
val delete: string -> path
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
val save : string -> path -> string
  -> [ `OK of unit
     | `KO of
         (** Something went wrong at the system level.
             (It may be git-related or os-related.) *)
         [> `SystemError of string
         ]
     ] Lwt.t

(** [append who path content] appends the string [content] at the end
    of the file located at [path]. If there is no file at [path], it
    is created. *)
val append : string -> path -> string
  -> [ `OK of unit
     | `KO of
         (** Something went wrong at the system level.
             (It may be git-related or os-related.) *)
         [> `SystemError of string
         ]
     ] Lwt.t

(** a [version] is a timestamped file. *)
type version

(** [versions path] returns the list of versions of the file
    located at [path]. *)
val versions : path
  -> [ `OK of version list
     | `KO of
         (** Something went wrong at the system level.
             (It may be git-related or os-related.) *)
         [> `SystemError of string
         ]
     ] Lwt.t

(** [latest id] is the latest version of the file
    located at [path]. *)
val latest : path
  -> [ `OK of version
     | `KO of
         (** Something went wrong at the system level.
             (It may be git-related or os-related.) *)
         [> `SystemError of string
         ]
     ] Lwt.t

(** [number version] returns a unique string identifier for the
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
     | `KO of
         (** Something went wrong at the system level.
             (It may be git-related or os-related.) *)
         [> `SystemError of string
         ]
     ] Lwt.t

(** [owner path] returns the path of the nearest englobing [subvfs]
    from [path]. *)
val owner : Identifier.path
  -> [ `OK of Identifier.path
     | `KO of
         (** Something went wrong at the system level.
             (It may be git-related or os-related.) *)
         [> `SystemError of string
         ]
     ] Lwt.t

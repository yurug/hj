(* -*- tuareg -*- *)

(** This module implements abstract types for paths and identifiers.

    Given a tree whose node are labelled, we characterize a node [n]
    in that tree by a [path] that is the sequence of labels crossed
    when traversing the tree from the root to [n].

    A subset of paths are singularized as the [identifier]s. We want
    the type system to help us distinguish between the universe of
    paths and this subset. For this reason, we introduced two abstract
    types and explicit conversion functions between them.
*)

(** A label is implemented as a string that does not contain
    {!Filename.dir_sep}. *)
type label = private string

(** [label s] returns a label represented by [s].
    Raise {!InvalidLabel} if [s] contains {!Filename.dir_sep}. *)
val label : string -> label
exception InvalidLabel of string

(** A path is a sequence of label from the root to a node. *)
type path = label list

(** An identifier is a singularized path. *)
type identifier

(** {2 Conversions} *)

val identifier_of_path : path -> identifier
val path_of_identifier : identifier -> path

(** {2 Utilities} *)

(** We reuse the host system's convention to represent
    paths as string. See {!Filename}. *)
val path_of_string : string -> path
val string_of_path : path -> string
val identifier_of_string : string -> identifier
val string_of_identifier : identifier -> string

module Map : Map.S with type key = identifier

val lwt_map_map
: ('a -> 'b Lwt.t) -> 'a Map.t -> 'b Map.t Lwt.t

val lwt_map_fold
: (Map.key -> 'a -> 'b -> 'b Lwt.t) -> 'a Map.t -> 'b -> 'b Lwt.t

module Set : Set.S with type elt = identifier

type identifiers = Set.t

(** [suffix prefix path] extracts the [prefix] of [path].
    Raise InvalidPrefix if [prefix] is not a prefix of [path]. *)
val suffix : path -> path -> path
exception InvalidPrefix of path * path

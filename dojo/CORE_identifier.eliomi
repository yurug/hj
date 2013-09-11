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
type label deriving (Json)

(** [label s] returns a label represented by [s].
    Raise {!InvalidLabel} if [s] contains {!Filename.dir_sep}. *)
val label : string -> label
val label_to_string : label -> string
exception InvalidLabel of string

(** A path is a sequence of label from the root to a node. *)
type path deriving (Json)

(** [make x] turns a list of labels into a path. *)
val make : label list -> path

(** An absolute path starts with an empty label. *)
val absolute : path -> bool

(** [concat x y] appends [y] to [x]. *)
val concat : path -> path -> path

(** [pcompare x y] must be used to compare two paths. *)
val pcompare : path -> path -> int

(** An identifier is a singularized path. *)
type identifier deriving (Json)
type t = identifier deriving (Json)

(** {2 Conversions} *)

val identifier_of_path : path -> identifier
val path_of_identifier : identifier -> path

(** {2 Utilities} *)

val hash : t -> int
val compare : t -> t -> int
val equal : t -> t -> bool

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

(** [suffix prefix path] removes the [prefix] of [path].
    Raise InvalidPrefix if [prefix] is not a prefix of [path]. *)
val suffix : path -> path -> path
exception InvalidPrefix of path * path

(** {2 Standard identifiers} *)

val root : bool -> path -> path

val tests_path : path

val users_path : path

val std_paths : path list

val fresh : path -> string -> identifier

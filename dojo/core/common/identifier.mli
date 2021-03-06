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
val fresh_label : string -> label
exception InvalidLabel of string

(** A path is a sequence of label from the root to a node. *)
type path deriving (Json)

(** [make x] turns a list of labels into a path. *)
val make : label list -> path
val from_strings : string list -> path

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
val string_list_of_identifier : identifier -> string list
val identifier_of_string_list : string list -> identifier

module Map : Map.S with type key = identifier

val lwt_map_map
: ('a -> 'b Lwt.t) -> 'a Map.t -> 'b Map.t Lwt.t

val lwt_map_fold
: (Map.key -> 'a -> 'b -> 'b Lwt.t) -> 'a Map.t -> 'b -> 'b Lwt.t

module Set : Set.S with type elt = identifier

module Dict : sig
  type 'a t deriving (Json)
  val empty : 'a t
  val add : identifier -> 'a -> 'a t -> 'a t
  val find : identifier -> 'a t -> 'a
  val iter : ((identifier * 'a) -> unit) -> 'a t -> unit
  val iter_s : ((identifier * 'a) -> unit Lwt.t) -> 'a t -> unit Lwt.t
  val fold_s : ('b -> (identifier * 'a) -> 'b Lwt.t) -> 'b -> 'a t -> 'b Lwt.t
end

type identifiers = Set.t

(** [is_prefix p id] returns [true] if [p] is a prefix of [id]. *)
val is_prefix : path -> identifier -> bool

(** [suffix prefix path] removes the [prefix] of [path].
    Raise InvalidPrefix if [prefix] is not a prefix of [path]. *)
val suffix : path -> path -> path
exception InvalidPrefix of path * path

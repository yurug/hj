(* -*- tuareg -*- *)

(** In-memory (purely functional) representation of entities state.

    The state of an entity contains:
    - specific data ;
    - a set of properties (see {!Property}) ;
    - a set of resources (see {!Resource}) ;
    - multi-sorted dependencies to other entities
     (a set of entities, a set of functions from entity to entity,
      a set of functions from a pair of entities to entity...).

    Moreover, a timestamp corresponds to the last time the state
    has been updated.

    All the operations on the entities state are atomic (with respect
    to Lwt), so they must be fast.
*)

open Identifier

(** {1 Definitions of entities. *)

(** The entity descriptor in memory. *)
type 'a meta deriving (Json)

(** The dependencies of an entity. *)
type dependencies

(** A low-level change to the state of an entity is of
    the following form. *)
type 'a state_change =
  | UpdateDependencies   of dependencies
  | UpdateResources      of Resource.t list
  | UpdateResourceStatus of Resource.name * bool
  | UpdateContent        of 'a
  | UpdateSequence       of 'a state_change * 'a state_change
  | NoUpdate

val state_changes : 'a state_change list -> 'a state_change

(** [identifier m] returns the identifier of [m]. *)
val identifier : 'a meta -> identifier

(** [content m] returns the content of [m]. *)
val content : 'a meta -> 'a

(** [resources m] returns the names of [m]'s resources. *)
val resources : 'a meta -> Resource.name list

(** [is_public_resource m r] returns the status of the resource [r]. *)
val is_public_resource : 'a meta -> Resource.name -> bool

(** [dependencies m] returns the dependencies of [m]. *)
val dependencies : 'a meta -> dependencies

(** [timestamp m] returns the timestamp of [m]. *)
val timestamp : 'a meta -> Timestamp.t

(** [now m] puts a timestamp on [m]. *)
val now : 'a meta -> 'a meta

(** Dependencies are classified using names. *)
type dependency_kind = string

(** A dependency [(y, (k, xs)] of kind [k] of an identifier [x] is a
    function from an a list of identifiers [xs] to an identifier [y].
    We usually write it [k (xs) = y]. *)
type dependency = identifier * (dependency_kind * identifier list)

(** No dependency. *)
val empty_dependencies : dependencies

(** [dependency_image ds] returns all the identifiers included in
    an identifier's dependencies [ds]. *)
val dependency_image : dependencies -> dependency list

(** [dependency d k xs] returns [Some (k (xs))] if there is
    a [y] in [d] such that [k (xs) = y]. Otherwise, returns
    [None]. *)
val dependency
  : dependencies -> dependency_kind -> identifier list -> identifier option

(** [all_dependencies d k] returns all the [(xs, y)] such that
    there is a [y] in [d] such [k (xs) = y]. *)
val all_dependencies
  : dependencies -> dependency_kind -> (identifier list * identifier) list

(** [push ds d] is the dependencies [ds] with an extra dependency [d]. *)
val push : dependencies -> dependency -> dependencies

(** [of_list ls] turns an associative list into dependencies. *)
val of_list:
  (dependency_kind * ((identifier list * identifier) list)) list
  -> dependencies

(** [to_list ds] transforms dependencies [ds] into an associative list. *)
val to_list:
  dependencies -> (dependency_kind * ((identifier list * identifier) list)) list

(** {2 Public API for the implementation of the entity engine} *)

(** [make id ds ps res c] returns an entity description for entity
    [id] with dependencies [ds], properties [ps], resources [res], and
    content [c]. *)
val make
  : identifier -> dependencies -> Resource.t list
  -> 'a -> 'a meta

(** [update e c] applies the low-level change [c] on the state
    of [e]. *)
val update : 'a meta -> 'a state_change -> 'a meta

val string_of_state_change : ('a -> string) -> 'a state_change -> string

val map : ('a -> 'b) -> 'a meta -> 'b meta

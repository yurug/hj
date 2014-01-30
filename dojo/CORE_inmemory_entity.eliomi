(* -*- tuareg -*- *)

(** In-memory representation of entities state.

    The state of an entity contains:
    - specific data ;
    - a set of properties (see {!Property}) ;
    - a set of source files ;
    - multi-sorted dependencies to other entities
     (a set of entities, a set of functions from entity to entity,
      a set of functions from a pair of entities to entity...).

    Moreover, a timestamp corresponds to the last time when the state
    has been updated.

    All the operations on the entities state are atomic (with respect
    to Lwt), so they must be fast.
*)

open CORE_identifier

(** {1 Public API for the definitions of entities. *)

(** The entity descriptor in memory. *)
type 'a meta deriving (Json)

(** The dependencies of an entity. *)
type dependencies

(** A low-level change to the state of an entity is of
    the following form. *)
type 'a state_change =
  | UpdateDependencies of dependencies
  | UpdateSources      of CORE_source.filename list
  | UpdateProperties   of CORE_property.set
  | UpdateContent      of 'a
  | UpdateSequence     of 'a state_change * 'a state_change
  | NoUpdate

val state_changes : 'a state_change list -> 'a state_change

(** [identifier m] returns the identifier of [m]. *)
val identifier : 'a meta -> identifier

(** [content m] returns the content of [m]. *)
val content : 'a meta -> 'a

(** [properties m] returns the properties of [m]. *)
val properties : 'a meta -> CORE_property.set

(** [sources m] returns the filenames of [m]'s sources. *)
val sources : 'a meta -> CORE_source.filename list

(** [dependencies m] returns the dependencies of [m]. *)
val dependencies : 'a meta -> dependencies

(** [timestamp m] returns the timestamp of [m]. *)
val timestamp : 'a meta -> float

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

(** [refresh d] makes sure that the references of [d] are still valid. *)
val refresh : 'a meta -> 'a meta

(** {2 Public API for the implementation of the entity engine} *)

(** [make id ds c] returns an entity description for entity
    [id] with dependencies [ds] and content [c]. *)
val make :
  identifier
  -> dependencies
  -> CORE_property.set
  -> CORE_source.filename list
  -> 'a -> 'a meta

(** [update e c] applies the low-level change [c] on the state
    of [e]. *)
val update : 'a meta -> 'a state_change -> 'a meta

val string_of_state_change : ('a -> string) -> 'a state_change -> string

val map : ('a -> 'b) -> 'a meta -> 'b meta

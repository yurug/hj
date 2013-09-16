(* -*- tuareg -*- *)

(** In-memory representation of entities data.

    The state of an entity contains:
    - specific data ;
    - multi-sorted dependencies to other entities
     (a set of entities, a set of functions from entity to entity,
      a set of functions from a pair of entities to entity...).
*)

open CORE_identifier

(** The entity descriptor in memory. *)
type 'a meta deriving (Json)

(** The dependencies of an entity. *)
type dependencies

(** [make id ds c] returns an entity description for entity
    [id] with dependencies [ds] and content [c]. *)
val make : identifier -> dependencies -> CORE_property.set -> 'a -> 'a meta

(** [identifier m] returns the identifier of [m]. *)
val identifier : 'a meta -> identifier

(** [content m] returns the content of [m]. *)
val content : 'a meta -> 'a

(** [properties m] returns the properties of [m]. *)
val properties : 'a meta -> CORE_property.set

(** [update_content m c] returns a new version of [m]
    such that [content m = c]. *)
val update_content : 'a meta -> 'a -> 'a meta

(** [update_properties m p] returns a new version of [m]
    such that [properties m = p]. *)
val update_properties : 'a meta -> CORE_property.set -> 'a meta

(** [dependencies m] returns the dependencies of [m]. *)
val dependencies : 'a meta -> dependencies

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

(** [push ds d] is the dependencies [ds] with an extra dependency [d]. *)
val push : dependencies -> dependency -> dependencies

(** [of_list ls] turns an associative list into dependencies. *)
val of_list:
  (dependency_kind * ((identifier list * identifier) list)) list
  -> dependencies

(** [to_list ds] transforms dependencies [ds] into an associative list. *)
val to_list:
  dependencies -> (dependency_kind * ((identifier list * identifier) list)) list

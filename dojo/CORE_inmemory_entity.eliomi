(* -*- tuareg -*- *)

(** In-memory representation of entities data.

    The state of an entity contains:
    - specific data ;
    - multi-sorted dependencies to other entities
     (a set of entities, a set of functions from entity to entity,
      a set of functions from a pair of entities to entity...) ;
*)

type 'a meta deriving (Json)

type dependencies

val empty_dependencies : dependencies

val dependency_image : dependencies -> CORE_identifier.t list

val of_list:
  (string * ((CORE_identifier.t list * CORE_identifier.t) list)) list
  -> dependencies

val make : CORE_identifier.t -> dependencies -> 'a -> 'a meta

val identifier : 'a meta -> CORE_identifier.t

val dependencies : 'a meta -> dependencies

val content : 'a meta -> 'a

val update_content : 'a meta -> 'a -> 'a meta

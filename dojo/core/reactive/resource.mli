(* -*- tuareg -*- *)

(** A resource has a name and a modifiable content.

    For the moment, the content is limited to store OCaml strings.
*)

type name = string deriving (Json)

type t

val empty : name -> t

val make : name -> string -> t

val set_content : t -> string -> unit

val content : t -> string

val name : t -> name

module Map : Map.S with type key = name

type map = t Map.t

val map_of_list : t list -> map

val list_of_map : map -> t list

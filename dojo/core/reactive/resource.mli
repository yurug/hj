(* -*- tuareg -*- *)

type filename = string deriving (Json)

type t

val empty : filename -> t

val make : filename -> string -> t

val set_content : t -> string -> unit

val content : t -> string

val filename : t -> filename

module Map : Map.S with type key = filename

type map = t Map.t

val map_of_list : t list -> map

val list_of_map : map -> t list

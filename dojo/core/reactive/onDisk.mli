(* -*- tuareg -*- *)

(** On-the-disk representation of entities.

    The state of an entity is frequently replicated on the disk. In
    case of crash, it should be possible to restore the entire graphs
    of entities from the disk.

    This on-the-disk representation consists of:

    (i) a description of the state as an JSON object ;
    (ii) a set of ressource files.

    We make use of {!VFS} to store the history of entities.

    For an entity named [id], the JSON object is stored in a ".meta"
    file.  Both this JSON file and the other related resources files
    are stored in the subvfs rooted at [id].

*)

module type S = sig

  type data

  val save : data InMemory.meta ->
    [ `OK of unit
    | `KO of [> `SystemError of string | `InternalError of exn]
    ] Lwt.t

  val load : Identifier.t ->
    [ `OK of data InMemory.meta
    | `KO of [>
      `UndefinedEntity of Identifier.t
    | `SystemError of string
    ]] Lwt.t

end

module type Converter = sig
  val version : string
  type source deriving (Json)
  type destination
  val convert : source -> destination
end

module Make (D : sig
  type data deriving (Json)
  val current_version : string
  val converters : (module Converter with type destination = data) list
end)
: S with type data = D.data

val save_resource : Identifier.t -> Resource.t ->
  [ `OK of bool
  | `KO of [> `SystemError of string ]
  ] Lwt.t

val load_resource : Identifier.t -> ?version:VFS.version -> Resource.name ->
  [ `OK of Resource.t
  | `KO of [> `SystemError of string ]
  ] Lwt.t

val resource_versions : Identifier.t -> Resource.name ->
  [ `OK of VFS.version list
  | `KO of [> `SystemError of string ]
  ] Lwt.t

val exists : Identifier.t -> bool

val timestamp : Identifier.t ->
  [ `OK of Int64.t
  | `KO of [>
      `UndefinedEntity of Identifier.t
    | `SystemError of string
  ]] Lwt.t

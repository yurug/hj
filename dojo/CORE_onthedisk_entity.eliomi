(* -*- tuareg -*- *)

(** On-the-disk representation of entities.

    The state of an entity always replicated on the disk. In case
    of crash, it should be possible to restore the entire graphs
    of entities from the disk.

    This on-the-disk representation consists of:

     (i) a description of the state as an JSON object ;
    (ii) a set of ressource files.

    We make use of {!CORE_vfs} to store the history of entities.

    For an entity named [id], the JSON object is stored in a ".meta"
    file.  Both this JSON file and the other related ressource files
    are stored in the subvfs rooted at [id].

*)

module type S = sig

  type data

  val save : data CORE_inmemory_entity.meta ->
    [ `OK of unit
    | `KO of [> `SystemError of string ]
    ] Lwt.t

  val load : CORE_identifier.t ->
    [ `OK of data CORE_inmemory_entity.meta
    | `KO of [>
      `UndefinedEntity of CORE_identifier.t
    | `SystemError of string
    ]] Lwt.t

end

module Make (D : sig type data deriving (Json) end)
: S with type data = D.data

val log : CORE_identifier.t -> string ->
  [ `OK of unit
  | `KO of [> `SystemError of string ]
  ] Lwt.t

val save_source : CORE_identifier.t -> CORE_source.t ->
  [ `OK of bool
  | `KO of [> `SystemError of string ]
  ] Lwt.t

val load_source : CORE_identifier.t -> CORE_source.filename ->
  [ `OK of CORE_source.t
  | `KO of [> `SystemError of string ]
  ] Lwt.t

val exists : CORE_identifier.t -> bool

val timestamp : CORE_identifier.t ->
  [ `OK of Int64.t
  | `KO of [>
      `UndefinedEntity of CORE_identifier.t
    | `SystemError of string
  ]] Lwt.t

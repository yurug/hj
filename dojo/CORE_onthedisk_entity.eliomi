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

module Make (D : sig type data deriving (Json) end) : sig

  val save : D.data CORE_inmemory_entity.meta -> CORE_source.t list ->
    [ `OK of unit
    | `KO of [> `SystemError of string ]
    ] Lwt.t

  val load : CORE_identifier.t ->
    [ `OK of D.data CORE_inmemory_entity.meta * CORE_source.t list
    | `KO of [>
      `UndefinedEntity of CORE_identifier.t
    | `SystemError of string
    ]] Lwt.t

  val exists : CORE_identifier.t -> bool

end

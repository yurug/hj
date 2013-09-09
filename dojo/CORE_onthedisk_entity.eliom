(* -*- tuareg -*- *)

(** On-the-disk representation of entities. *)

open Lwt

open COMMON_pervasives
open CORE_inmemory_entity
open CORE_identifier

module Make (D : sig type data deriving (Json) end) = struct

  let who = "system.onthedisk_entity"

  let metafile id =
    let path = path_of_identifier id in
    concat path (make [label ".meta.json"])

  let save (m : D.data meta) =
    let raw = Deriving_Json.to_string Json.t<D.data meta> m in
    CORE_vfs.save who ~relative:true (metafile (identifier m)) raw

  let load id =
    CORE_vfs.latest ~relative:true (metafile id)
    >>>= fun latest_version -> CORE_vfs.read latest_version
    >>>= fun raw ->
    return (`OK (Deriving_Json.from_string Json.t<D.data meta> raw))

  let exists id =
    assert false

end

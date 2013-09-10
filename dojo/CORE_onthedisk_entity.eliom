(* -*- tuareg -*- *)

(** On-the-disk representation of entities. *)

open Lwt

open COMMON_pervasives
open CORE_inmemory_entity
open CORE_identifier

module Make (D : sig type data deriving (Json) end) = struct

  let who = "system.onthedisk_entity <here@hackojo.org>"

  let metafile id =
    let path = path_of_identifier id in
    concat path (make [label ".meta.json"])

  let save (m : D.data meta) =
    let raw = Deriving_Json.to_string Json.t<D.data meta> m in
    let id = identifier m in
    (if not (Sys.file_exists (string_of_identifier id)) then
       CORE_vfs.create who ~relative:true (path_of_identifier id)
        >>= function
          | `KO (`AlreadyExists _) -> assert false
          | `KO (`SystemError e) -> return (`KO (`SystemError e))
          | `OK x -> return (`OK x)
    else
      return (`OK ())
    ) >>>= fun () ->
    CORE_vfs.save who ~relative:true (metafile id) raw

  let load id =
    CORE_vfs.latest ~relative:true (metafile id)
    >>>= fun latest_version -> CORE_vfs.read latest_version
    >>>= fun raw ->
    return (`OK (Deriving_Json.from_string Json.t<D.data meta> raw))

  let exists id =
    Sys.file_exists (string_of_path (metafile id))

end

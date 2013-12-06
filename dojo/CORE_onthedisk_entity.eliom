(* -*- tuareg -*- *)

(** On-the-disk representation of entities. *)

open Lwt
open Deriving_Json

open COMMON_pervasives
open CORE_inmemory_entity
open CORE_identifier
open CORE_standard_identifiers

let file path fname =
  concat path (make [label fname])

let metafile path =
  file path ".meta.json"

let timestamp id =
  let path = path_of_identifier id in
  CORE_vfs.latest (metafile path) >>>= (function v ->
    lwt timestamp = CORE_vfs.timestamp v in
    return (`OK timestamp)
  )

module Make (D : sig type data deriving (Json) end) = struct

  let who = "system.onthedisk_entity <here@hackojo.org>"

  let save (m : D.data meta) =
    let raw = to_string Json.t<D.data meta> m in
    let id = identifier m in
    let path = root true (path_of_identifier id) in
    (if not (Sys.file_exists (string_of_path path)) then
       CORE_vfs.create who ~relative:false path
        >>= function
          | `KO (`AlreadyExists _) -> assert false
          | `KO (`SystemError e) -> return (`KO (`SystemError e))
          | `OK x -> return (`OK x)
    else
      return (`OK ())
    )
    >>>= fun _ -> CORE_vfs.save who ~relative:false (metafile path) raw
    >>>= fun _ -> return (`OK ())

  let exists id =
    let path = root true (path_of_identifier id) in
    Sys.file_exists (string_of_path (metafile path))

  let load id =
    let path = path_of_identifier id in
    (if not (exists id) then
        return (`KO (`UndefinedEntity id))
     else
        return (`OK ()))
    >>>= (fun () -> CORE_vfs.latest (metafile path))
    >>>= fun latest_version -> CORE_vfs.read latest_version
    >>>= fun raw ->
    let meta = from_string Json.t<D.data meta> raw in
    return (`OK meta)

  let log id what =
    let path = path_of_identifier id in
    CORE_vfs.append who (file path ".log") what

end

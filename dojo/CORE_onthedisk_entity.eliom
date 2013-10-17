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

  let save (m : D.data meta) (ss : CORE_source.t list) =
    let raw = to_string Json.t<D.data meta> m in
    let id = identifier m in
    let path = root true (path_of_identifier id) in
    let save_source s =
      (* FIXME: Do that only if the source has been modified. *)
      let fname = file path (CORE_source.filename s) in
      CORE_vfs.save who ~relative:false fname (CORE_source.content s)
      (* FIXME: We ignore errors at this point because we should instead try
         to log all the errors and present them in an uniform way. *)
      >>= function _ -> return (`OK ())
    in
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
    >>>= fun _ -> list_map_s save_source ss
    >>>= fun _ -> return (`OK ())

  let exists id =
    let path = root true (path_of_identifier id) in
    Sys.file_exists (string_of_path (metafile path))

  let load id =
    let path = path_of_identifier id in
    let load_source fname =
      CORE_vfs.latest (concat path (make [label fname]))
      >>>= fun latest_version -> CORE_vfs.read latest_version
      >>>= fun raw -> return (`OK (CORE_source.make fname raw))
    in
    (if not (exists id) then
        return (`KO (`UndefinedEntity id))
     else
        return (`OK ()))
    >>>= (fun () -> CORE_vfs.latest (metafile path))
    >>>= fun latest_version -> CORE_vfs.read latest_version
    >>>= fun raw ->
    let meta = from_string Json.t<D.data meta> raw in
    list_map_s load_source (sources meta)
    >>>= fun sources ->
    return (`OK (meta, sources))

end

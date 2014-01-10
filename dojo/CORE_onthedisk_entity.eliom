(* -*- tuareg -*- *)

(** On-the-disk representation of entities. *)

open Lwt
open Deriving_Json

open COMMON_pervasives
open CORE_inmemory_entity
open CORE_identifier
open CORE_standard_identifiers

let who = "system.onthedisk_entity <here@hackojo.org>"

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

let load_source id fname =
  let path = root true (path_of_identifier id) in
  CORE_vfs.latest ~relative:false (file path fname)
  >>= function
    | `KO e ->
      return (`KO e)
    | `OK latest_version ->
      CORE_vfs.read latest_version
      >>= function
        | `KO e ->
          return (`KO e)
        | `OK content ->
          return (`OK (CORE_source.make fname content))

let save_source id s =
  let save () =
    let path = root true (path_of_identifier id) in
    CORE_vfs.save who ~relative:false
      (file path (CORE_source.filename s))
      (CORE_source.content s)
    >>>= fun _ -> return (`OK true)
  in
  load_source id (CORE_source.filename s) >>= function
    | `OK ls ->
      if ls = s then
        return (`OK false)
      else
        save ()
    | `KO _ ->
      save ()


let exists id =
  let path = root true (path_of_identifier id) in
  Sys.file_exists (string_of_path (metafile path))

let log id what =
  let path = path_of_identifier id in
  CORE_vfs.append who (file path ".log") what

module Make (D : sig type data deriving (Json) end)
: S with type data = D.data = struct

  type data = D.data

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
    let meta = CORE_inmemory_entity.refresh meta in
    return (`OK meta)

end

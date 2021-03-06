(* -*- tuareg -*- *)

(** On-the-disk representation of entities. *)

open Lwt
open Deriving_Json

open ExtPervasives
open InMemory
open Identifier

let who i =
  Printf.sprintf
    "%s <here@hackojo.org>"
    Str.(global_replace (regexp "/") "." (string_of_identifier i))

module type S = sig

  type data

  val save : data InMemory.meta ->
    [ `OK of unit
    | `KO of [> `SystemError of string | `InternalError of exn]
    ] Lwt.t

  val history : Identifier.t -> VFS.version list Lwt.t

  val load : ?version:VFS.version -> Identifier.t ->
    [ `OK of data InMemory.meta
    | `KO of [>
      `UndefinedEntity of Identifier.t
    | `SystemError of string
    ]] Lwt.t

end

let file path fname =
  concat path (make [label fname])

let metafile path =
  file path ".meta.json"

let timestamp id =
  VFS.latest (metafile (path_of_identifier id)) >>>= (function v ->
    lwt timestamp = VFS.timestamp v in
    return (`OK timestamp)
  )

let resource_real_path id fname =
  VFS.real_path (file (path_of_identifier id) fname)

let load_resource id ?version fname =
  let path = file (path_of_identifier id) fname in
  (match version with
    | None -> VFS.latest path
    | Some version_number -> VFS.version_from_number path version_number
  ) >>= function
    | `KO e ->
      return (`KO e)
    | `OK version ->
      VFS.read version >>= function
        | `KO e ->
          return (`KO e)
        | `OK content ->
          return (`OK (Resource.make fname content, path))

let resource_versions id name =
  VFS.versions (file (path_of_identifier id) name)

let save_resource id s on_finished =
  let save () = (
    let path = file (path_of_identifier id) (Resource.name s) in
    VFS.save (who id) path (Resource.content s) on_finished
    >> return (`OK true)
  ) >>= function
    | `OK r -> return (`OK r)
    | `KO r  -> Lwt.async on_finished; return (`KO r)
  in
  load_resource id (Resource.name s) >>= function
    | `OK (ls, _) ->
      if ls = s then (
        Lwt.async on_finished;
        return (`OK false)
      )
      else
        save ()
    | `KO _ ->
      save ()

let commit_resource id s on_finished =
  let path = file (path_of_identifier id) (Resource.name s) in
  VFS.commit (who id) path on_finished

let exists id =
  VFS.exists (metafile (path_of_identifier id))

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
: S with type data = D.data = struct

  let ondisk_entity_descriptor =
    Log.make_event_descriptor "ondisk_entity" Facts.string

  let log id msg =
    Log.log_string id ondisk_entity_descriptor msg

  type data = D.data

  type versioned_data = {
    version : string;
    content : string;
  } deriving (Json)

  let save (m : D.data meta) =
    let versioned = {
      version = D.current_version;
      content = to_string Json.t<D.data meta> m
    }
    in
    let raw = to_string Json.t<versioned_data> versioned in
    let id = identifier m in
    let path = path_of_identifier id in
    (if not (VFS.exists path) then
       VFS.create (who id) path >>= function
         | `KO (`AlreadyExists _) ->
           (* Absurd because of VFS.exists. *)
           (try assert false with e -> return (`KO (`InternalError e)))
         | `KO (`SystemError e) ->
           return (`KO (`SystemError e))
         | `OK x ->
           return (`OK x)
     else
      return (`OK ())
    )
    >>>= fun _ -> VFS.save (who id) (metafile path) raw (fun () -> return ())
    >>>= fun _ -> return (`OK ())

  let rec convert vd = function
    | [] ->
      return (`KO (`SystemError ("No converter for version " ^ vd.version)))
    | (module M : Converter with type destination = D.data) :: ms ->
      if M.version = vd.version then (
        let meta = from_string Json.t<M.source meta> vd.content in
        let meta = InMemory.map M.convert meta in
        return (`OK meta)
      )
      else convert vd ms

  let history id =
    VFS.versions (metafile (path_of_identifier id)) >>= function
      | `OK vs -> return vs
      | `KO _ -> return [] (* FIXME: This should never happen. *)

  let load ?version id =
    let path = path_of_identifier id in
    (if not (exists id) then
        return (`KO (`UndefinedEntity id))
     else
        return (`OK ()))
    >>>= (fun () ->
      match version with
        | None -> VFS.latest (metafile path)
        | Some v -> return (`OK v))
    >>>= VFS.read
    >>>= fun raw ->
    try_lwt
      let vd = from_string Json.t<versioned_data> raw in
      if vd.version = D.current_version then
        return (`OK (from_string Json.t<D.data meta> vd.content))
      else
      (** The stored file does not contain data of the current version.
          We convert it on-the-fly. *)
        convert vd D.converters
    with _ ->
      let msg = "Problem when reading " ^ string_of_identifier id in
      log id msg;
      return (`KO (`SystemError msg))


end

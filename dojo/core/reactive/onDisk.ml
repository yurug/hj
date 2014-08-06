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

  val load : Identifier.t ->
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

let load_resource id fname =
  VFS.latest (file (path_of_identifier id) fname)
  >>= function
    | `KO e ->
      return (`KO e)
    | `OK latest_version ->
      VFS.read latest_version
      >>= function
        | `KO e ->
          return (`KO e)
        | `OK content ->
          return (`OK (Resource.make fname content))

let save_resource id s =
  let save () =
    let path = file (path_of_identifier id) (Resource.name s) in
    VFS.save (who id) path (Resource.content s)
    >>>= fun _ -> return (`OK true)
  in
  load_resource id (Resource.name s) >>= function
    | `OK ls ->
      if ls = s then
        return (`OK false)
      else
        save ()
    | `KO _ ->
      save ()

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
    >>>= fun _ -> VFS.save (who id) (metafile path) raw
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

  let load id =
    let path = path_of_identifier id in
    (if not (exists id) then
        return (`KO (`UndefinedEntity id))
     else
        return (`OK ()))
    >>>= (fun () -> VFS.latest (metafile path))
    >>>= VFS.read
    >>>= fun raw ->
    let vd = from_string Json.t<versioned_data> raw in
    if vd.version = D.current_version then
      return (`OK (from_string Json.t<D.data meta> vd.content))
    else
      (** The stored file does not contain data of the current version.
          We convert it on-the-fly. *)
      convert vd D.converters

end

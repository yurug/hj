(* -*- tuareg -*- *)

(** In-memory representation of entities content. *)

open CORE_identifier

type dependency_kind = string

type dependency = identifier * (dependency_kind * identifier list)

(* FIXME: Optimize that naive representation if needed. *)
type dependencies =
    (string * ((CORE_identifier.t list * CORE_identifier.t) list)) list
    deriving (Json)

let empty_dependencies =
  []

let dependency_image (x : dependencies) =
  List.flatten (List.map (fun (l, rel) ->
    List.map (fun (xs, y) -> (y, (l, xs))) rel)
  x)

let push (deps : dependencies) ((y, (l, xs)) : dependency) =
  let rel = try List.assoc l deps with Not_found -> [] in
  let deps = List.remove_assoc l deps in
  let rel = if List.mem (xs, y) rel then rel else (xs, y) :: rel in
  (l, rel) :: deps

let of_list x = x

let to_list x = x

type 'a meta = {
  identifier      : CORE_identifier.t;
  dependencies    : dependencies;
  properties      : CORE_property.set;
  sources         : CORE_source.filename list;
  content         : 'a;
  tick            : int;
} deriving (Json)

let make identifier dependencies properties sources content =
  { identifier; dependencies; content; properties; sources; tick = 0 }

let sources e = e.sources

let identifier e = e.identifier

let dependencies e = e.dependencies

let properties e = e.properties

let content e = e.content

let update e = { e with tick = e.tick + 1 }

let update_content e c = update { e with content = c }

let update_properties e s = update { e with properties = s }

let update_dependencies e d = update { e with dependencies = d }

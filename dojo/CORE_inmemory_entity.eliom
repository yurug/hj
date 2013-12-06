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

let dependency d k xs =
  try
    Some (List.assoc xs (List.assoc k d))
  with Not_found -> None

type 'a meta = {
  identifier      : CORE_identifier.t;
  dependencies    : dependencies;
  properties      : CORE_property.set;
  sources         : CORE_source.filename list;
  content         : 'a;
  tick            : float;
} deriving (Json)

let now e = { e with tick = Unix.gettimeofday () }

let make identifier dependencies properties sources content =
  now { identifier; dependencies; content; properties; sources; tick = 0. }

let sources e = e.sources

let identifier e = e.identifier

let dependencies e = e.dependencies

let properties e = e.properties

let content e = e.content

let timestamp e = e.tick

let update_content e c = now { e with content = c }

let update_properties e s = now { e with properties = s }

let update_dependencies e d = now { e with dependencies = d }

let update_sources e sources = now { e with sources }

type 'a change =
  | UpdateDependencies of dependencies
  | UpdateSources      of CORE_source.filename list
  | UpdateProperties   of CORE_property.set
  | UpdateContent      of 'a
  | UpdateSequence     of 'a change * 'a change
  | NoUpdate

let rec update e = function
  | UpdateDependencies d -> update_dependencies e d
  | UpdateSources s -> update_sources e s
  | UpdateProperties s -> update_properties e s
  | UpdateContent c -> update_content e c
  | UpdateSequence (c1, c2) -> update e c1; update e c2
  | NoUpdate -> e

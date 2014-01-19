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

let all_dependencies d k =
  try
    List.assoc k d
  with Not_found -> []

type 'a meta = {
  identifier      : CORE_identifier.t;
  dependencies    : dependencies;
  properties      : CORE_property.set;
  sources         : CORE_source.filename list;
  content         : 'a;
  tick            : float;
} deriving (Json)

let refresh e =  List.(CORE_standard_identifiers.(
  let refresh_dependency (k, ds) =
    let ds = filter (fun (_, y) -> ping y) ds in
    let ds = map (fun (xs, y) -> (filter ping xs, y)) ds in
    (k, ds)
  in
  let dependencies = List.map refresh_dependency e.dependencies in
  { e with dependencies }
))

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

let update_sources e s =
  let sources = s @ List.(filter (fun x -> not (mem x s)) e.sources) in
  now { e with sources }

type 'a state_change =
  | UpdateDependencies of dependencies
  | UpdateSources      of CORE_source.t list
  | UpdateProperties   of CORE_property.set
  | UpdateContent      of 'a
  | UpdateSequence     of 'a state_change * 'a state_change
  | NoUpdate

let rec state_changes = function
  | [] ->
    NoUpdate
  | NoUpdate :: cs ->
    state_changes cs
  | UpdateSequence (NoUpdate, c) :: cs ->
    state_changes (c :: cs)
  | UpdateSequence (c1, c2) :: cs ->
    UpdateSequence (c1, state_changes (c2 :: cs))
  | c :: cs ->
    UpdateSequence (c, state_changes cs)

let rec update e = function
  | UpdateDependencies d -> update_dependencies e d
  | UpdateSources s -> update_sources e (List.map CORE_source.filename s)
  | UpdateProperties s -> update_properties e s
  | UpdateContent c -> update_content e c
  | UpdateSequence (c1, c2) -> update (update e c1) c2
  | NoUpdate -> e

let rec string_of_state_change string_of_replacement = function
  | UpdateDependencies _ ->
    "Update dependencies"
  | UpdateSources s ->
    Printf.sprintf "Update sources (%s)"
      (String.concat " " (List.map CORE_source.filename s))
  | UpdateProperties s ->
    "Update properties"
  | UpdateContent young ->
    "Update content"
  | UpdateSequence (c1, NoUpdate) | UpdateSequence (NoUpdate, c1)->
    string_of_state_change string_of_replacement c1
  | UpdateSequence (c1, c2) ->
    string_of_state_change string_of_replacement c1 ^ "; "
    ^ string_of_state_change string_of_replacement c2
  | NoUpdate ->
    "No update"

let map f m = { m with content = f m.content }

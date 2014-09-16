(* -*- tuareg -*- *)

(** In-memory representation of an entity state. *)

open Identifier

type dependency_kind = string

type dependency = identifier * (dependency_kind * identifier list)

(* FIXME: Optimize that naive representation if needed. *)
type dependencies =
    (string * ((identifier list * identifier) list)) list
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
  identifier       : identifier;
  dependencies     : dependencies;
  resources        : Resource.name list;
  public_resources : Resource.name list;
  content          : 'a;
  tick             : Timestamp.t;
} deriving (Json)

let now e = { e with tick = Timestamp.current () }

let make identifier dependencies resources content =
  let resources = List.map Resource.name resources in
  let public_resources = [] in
  { identifier; dependencies; content; resources; public_resources;
    tick = Timestamp.current () }

let resources e = e.resources

let identifier e = e.identifier

let dependencies e = e.dependencies

let content e = e.content

let timestamp e = e.tick

let update_content e c = now { e with content = c }

let update_dependencies e d = now { e with dependencies = d }

let update_resources e s =
  let ns = List.map Resource.name s in
  let resources = ns @ List.(filter (fun x -> not (mem x ns)) e.resources) in
  now { e with resources }

let update_resource_status e r s =
  let public_resources =
    if s && not (List.mem r e.public_resources) then
      r :: e.public_resources
    else if not s then
      List.filter (fun r' -> r <> r') e.public_resources
    else
      e.public_resources
  in
  now { e with public_resources }

let is_public_resource e r =
  List.mem r e.public_resources

type 'a state_change =
  | UpdateDependencies   of dependencies
  | UpdateResources      of Resource.t list
  | UpdateResourceStatus of Resource.name * bool
  | UpdateContent        of 'a
  | UpdateSequence       of 'a state_change * 'a state_change
  | NoUpdate
(* FIXME: If state changes happen to be too large, we will move
   to a more intentional description, using diffs. *)

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
  | UpdateResources s -> update_resources e s
  | UpdateContent c -> update_content e c
  | UpdateSequence (c1, c2) -> update (update e c1) c2
  | UpdateResourceStatus (r, s) -> update_resource_status e r s
  | NoUpdate -> e

let rec string_of_state_change string_of_replacement = function
  | UpdateDependencies _ ->
    "Update dependencies"
  | UpdateResources s ->
    Printf.sprintf
      "Update sources (%s)"
      (String.concat " " (List.map Resource.name s))
  | UpdateContent young ->
    "Update content"
  | UpdateSequence (c1, NoUpdate) | UpdateSequence (NoUpdate, c1)->
    string_of_state_change string_of_replacement c1
  | UpdateSequence (c1, c2) ->
    string_of_state_change string_of_replacement c1 ^ "; "
    ^ string_of_state_change string_of_replacement c2
  | UpdateResourceStatus (r, s) ->
    (if s then "Publish " else "Unpublish ") ^ r
  | NoUpdate ->
    "No update"

let map f m = { m with content = f m.content }

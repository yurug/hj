(* -*- tuareg -*- *)

(** In-memory representation of entities content. *)

type dependencies =
    (string * ((CORE_identifier.t list * CORE_identifier.t) list)) list
    deriving (Json)

let empty_dependencies =
  []

let dependency_image (x : dependencies) =
  List.flatten (List.map (fun (l, rel) ->
    List.map (fun (xs, y) -> (y, (l, xs))) rel)
  x )

let push deps (l, (xs, y)) =
  let rel = try List.assoc l deps with Not_found -> [] in
  let deps = List.remove_assoc l deps in
  (l, (xs, y) :: rel) :: deps

let of_list x = x

let to_list x = x

type 'a meta = {
  identifier   : CORE_identifier.t;
  dependencies : dependencies;
  content         : 'a;
} deriving (Json)

let make identifier dependencies content = { identifier; dependencies; content }

let identifier e = e.identifier

let dependencies e = e.dependencies

let content e = e.content

let update_content e c = { e with content = c }

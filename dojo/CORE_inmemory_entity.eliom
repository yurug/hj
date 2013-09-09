(* -*- tuareg -*- *)

(** In-memory representation of entities content. *)

type dependencies =
    (string * ((CORE_identifier.t list * CORE_identifier.t) list)) list
    deriving (Json)

let empty_dependencies =
  []

let dependency_image (x : dependencies) =
  List.map snd (List.flatten (List.map snd x))

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

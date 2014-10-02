(* -*- tuareg -*- *)

(* FIXME: Enable lazy loading of content. *)

type name = string deriving (Json)

type t = {
  name : name;
  content : string ref;
}
deriving (Json)

let empty name = { name; content = ref "" }

let make name content_value = { name; content = ref content_value }

let set_content s c = s.content := c

let content s = !(s.content)

let name s = s.name

module Map = Map.Make (String)

type map = t Map.t

let map_of_list ss =
  List.fold_left (fun m s -> Map.add s.name s m) Map.empty ss

let list_of_map m =
  snd (List.split (Map.bindings m))

(* -*- tuareg -*- *)

type name = string deriving (Json)

type t = {
  name : name;
  mutable content : string;
}

let empty name = { name; content = "" }

let make name content = { name; content }

let set_content s c = s.content <- c

let content s = s.content

let name s = s.name

module Map = Map.Make (String)

type map = t Map.t

let map_of_list ss =
  List.fold_left (fun m s -> Map.add s.name s m) Map.empty ss

let list_of_map m =
  snd (List.split (Map.bindings m))

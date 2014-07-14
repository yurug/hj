(* -*- tuareg -*- *)

{shared{

type filename = string deriving (Json)

}}

type t = {
  filename : filename;
  mutable content  : string;
}

let empty filename = { filename; content = "" }

let make filename content = { filename; content }

let set_content s c = s.content <- c

let content s = s.content

let filename s = s.filename

module Map = Map.Make (String)

type map = t Map.t

let map_of_list ss =
  List.fold_left (fun m s -> Map.add s.filename s m) Map.empty ss

let list_of_map m =
  snd (List.split (Map.bindings m))

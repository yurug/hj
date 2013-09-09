(* -*- tuareg -*- *)

(* FIXME: This module implementation is naive. *)

open Lwt

type label = string deriving (Json)

exception InvalidLabel of string

let label s =
  try
    ignore (Str.search_forward (Str.regexp Filename.dir_sep) s 0);
    raise (InvalidLabel s)
  with Not_found ->
    s

let label_to_string s = s

type path = label list deriving (Json)

let rec normalize = function
  | [] -> []
  | [x] -> [x]
  | "" :: "" :: ls -> normalize ("" :: ls)
  | x :: ls -> x :: normalize ls

let pcompare x y = compare x y

type identifier = path deriving (Json)

type t = identifier deriving (Json)

let path_of_string s =
  normalize (Str.split_delim (Str.regexp Filename.dir_sep) s)

let ( // ) x y =
  if x = "" then
    Filename.dir_sep ^ y
  else
    Filename.concat x y

let make s = normalize s

let absolute = function
  | "" :: _ -> true
  | _ -> false

let concat x y = normalize (x @ y)

let ( /// ) p ps = List.fold_left ( // ) p ps

let string_of_path p = "" /// p

let identifier_of_path path = path

let path_of_identifier path = path

let string_of_identifier = string_of_path

let identifier_of_string = path_of_string

let compare = pcompare

let equal x y = (compare x y = 0)

module Compare = struct
  type t = identifier
  let compare = compare
end

let hash = Hashtbl.hash

module Set = Set.Make (Compare)

module Map = Map.Make (Compare)

let lwt_map_map f m =
  Map.fold (fun k v m ->
    lwt m = m in
    lwt u = f v in
    return (Map.add k u m)
  ) m (return Map.empty)

let lwt_map_fold f m init =
  Map.fold (fun k v x  ->
    lwt x = x in
    lwt x = f k v x in
    return x
  ) m (return init)

type identifiers = Set.t

exception InvalidPrefix of path * path

let rec suffix prefix p =
  match prefix, p with
  | [], p -> p
  | p :: ps, x :: xs when p = x -> suffix ps xs
  | _, _ -> raise Not_found

let suffix prefix p =
  try
    suffix prefix p
  with Not_found ->
    raise (InvalidPrefix (prefix, p))

let tests_path =
  [ "tests" ]

let fresh =
  let r = ref 0 in
  fun path p -> incr r; path @ [ p ^ string_of_int !r ]

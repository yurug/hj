(* -*- tuareg -*- *)

{shared{

(* FIXME: This module implementation is naive. *)

open Lwt

type label = string deriving (Json)

exception InvalidLabel of string

let label s =
  if String.contains s (Filename.dir_sep.[0]) then
    raise (InvalidLabel s)
  else
    s

let label_to_string s = s

(* FIXME: To be correct, we must store all the labels
   FIXME: crossed until now. *)
let fresh_label =
  let r = ref 0 in
  fun prefix -> incr r; prefix ^ string_of_int !r

type path = label list deriving (Json)

let rec normalize = function
  | [] -> []
  | [x] -> [x]
  | "" :: "" :: ls -> normalize ("" :: ls)
  | x :: ls -> x :: normalize ls

type identifier = path deriving (Json)

type t = identifier deriving (Json)

let split_delim c s =
  let b = Buffer.create 13 in
  let push c = Buffer.add_char b c in
  let flush () = let s = Buffer.contents b in Buffer.clear b; s in
  let rec aux parts i =
    if i = String.length s then
      let s = flush () in
      List.rev (s :: parts)
    else if s.[i] = c then
      let s = flush () in
      aux (s :: parts) (i + 1)
    else (
      push s.[i];
      aux parts (i + 1)
    )
  in
  aux [] 0

let path_of_string s =
  normalize (split_delim Filename.dir_sep.[0] s)

let ( // ) x y =
  if x = "" then
    Filename.dir_sep ^ y
  else
    Filename.concat x y

let make s = normalize s

let from_strings s = make s

let absolute = function
  | "" :: _ -> true
  | _ -> false

let concat x y = normalize (x @ y)

let ( /// ) p ps = List.fold_left ( // ) p ps

let string_of_path p = "" /// p

let pcompare x y = compare (string_of_path x) (string_of_path y)

let identifier_of_path path = path

let path_of_identifier path = path

let string_of_identifier = string_of_path

let identifier_of_string = path_of_string

let string_list_of_identifier x = x

let identifier_of_string_list ls = make (List.map label ls)

let compare = pcompare

let equal x y = (compare x y = 0)

module Compare = struct
  type t = identifier
  let compare = compare
end

let hash k = Hashtbl.hash (string_of_path k)

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

let is_prefix prefix p =
  try
    ignore (suffix prefix p); true
  with Not_found -> false

let suffix prefix p =
  try
    suffix prefix p
  with Not_found ->
    raise (InvalidPrefix (prefix, p))

}}

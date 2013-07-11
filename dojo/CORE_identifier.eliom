(* -*- tuareg -*- *)

(* FIXME: This module implementation is naive. *)

open Lwt

type label = string

exception InvalidLabel of string

let label s =
  if String.contains s Filename.dir_sep then raise (InvalidLabel s);
  s

type path = label list

type identifier = path

let path_of_string s =
  List.filter (fun l -> l <> "") (Str.split (Str.regexp Filename.dir_sep) s)

let ( // ) = Filename.concat

let ( /// ) p ps = List.fold_left ( // ) p ps

let string_of_path p = "" /// p

let identifier_of_path path = path

let path_of_identifier path = path

let string_of_identifier = string_of_path

let identifier_of_string = path_of_string

let compare_identifier = Pervasives.compare

module Compare = struct
  type t = identifier
  let compare = compare_identifier
end

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

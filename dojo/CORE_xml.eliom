(* -*- tuareg -*- *)

open Simplexmlparser
open Printf

exception InvalidXMLIO of string

let load_xml fname =
  try
    List.hd (xmlparser_file fname)
  with Failure s -> raise (InvalidXMLIO s)

exception InvalidPath of string list * xml

let plug path0 outer what =
  let lookup fs = try Some (List.assoc "id" fs) with Not_found -> None in
  let rec plug path e =
    match path, e with
    | [], e -> what
    | path, Element (t, fs, cs) ->
      Element (t, fs, plug_in_children path cs)
    | _, _ ->
      raise (InvalidPath (path0, outer))
  and plug_in_children path cs =
    match cs, path with
    | [], _ -> raise (InvalidPath (path0, outer))
    | (Element (_, fs, _) as e) :: cs, x :: xs when lookup fs = Some x ->
      plug xs e :: cs
    | _ :: es, path ->
      plug_in_children path es
  in
  plug path0 outer

let print_xml xml =
  let b = Buffer.create 13 in
  let field spaces (k, v) = Printf.sprintf "\n%s%s=\"%s\"" spaces k v in
  let fields spaces fs = String.concat " " (List.map (field spaces) fs) in
  let open_element spaces e fs simple =
    let close = if simple then "/>" else ">" in
    bprintf b "%s<%s %s%s\n" spaces e (fields (spaces ^ "  ") fs) close
  in
  let rec aux spaces = function
    | Element (e, fs, []) ->
      open_element spaces e fs true
    | Element (e, fs, cs) ->
      open_element spaces e fs false;
      aux' ("  " ^ spaces) cs;
      bprintf b "%s</%s>\n" spaces e
    | PCData s ->
      Buffer.add_string b (s ^ "\n")
  and aux' spaces cs = List.iter (aux spaces) cs in
  aux "" xml;
  b

let save_xml fname xml =
  try
    let cout = open_out fname in
    Buffer.output_buffer cout (print_xml xml);
    close_out cout
  with Failure s -> raise (InvalidXMLIO s)

(* -*- tuareg -*- *)

open Lwt
open Eliom_content.Html5.D

{shared{

type elt = [ Html5_types.flow5 ] Eliom_content.Html5.D.elt

type idx = int deriving (Json)

}}

{server{

let local_elt_table = Hashtbl.create 13

let rec local_push (elt : elt) =
  let x = Random.bits () in
  if Hashtbl.mem local_elt_table x then
    local_push elt
  else
    (Hashtbl.add local_elt_table x elt; x)

let local_get x =
  try
    let elt = Hashtbl.find local_elt_table x in
    Hashtbl.remove local_elt_table x;
    return elt
  with Not_found ->
    return (div [pcdata "Internal error"] (* FIXME *))

}}

{client{

let remote_get = %(server_function Json.t<int> local_get)

}}

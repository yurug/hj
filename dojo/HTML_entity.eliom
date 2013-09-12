(* -*- tuareg -*- *)

(** Entities HTML pages indexed by entities identifiers. *)

open Lwt
open Eliom_content.Html5.D
open Html5_types

open CORE_identifier

type page_maker =
    (identifier -> bool) * (identifier -> [ body_content ] elt Lwt.t)

let page_makers : page_maker list ref =
  ref []

let register_page_maker detect retrieve =
  page_makers := (detect, retrieve) :: !page_makers

let empty_page =
  div []

let get_page id =
  try_lwt
    let (_, retrieve) =
      List.find (fun (detect, _) -> detect id) !page_makers
    in
    retrieve id
  with Not_found ->
    return empty_page

let () =
  HTML_app.Hackojo_app.register
    ~secure_session:true
    ~service:HTTP_services.page_of
    (fun id () ->
      let id = make (List.map label id) in
      lwt page = get_page (identifier_of_path id) in
      HTML_app.hackojo_page [page]
    )

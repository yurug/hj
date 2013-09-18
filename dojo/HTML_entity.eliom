(* -*- tuareg -*- *)

(** Entities HTML pages indexed by entities identifiers. *)

open Lwt
open Eliom_content.Html5.D
open Html5_types
open Eliom_service

open HTML_widget
open HTML_app
open CORE_identifier

type page_maker =
    (identifier -> bool) * (identifier -> [ body_content ] elt Lwt.t)

let page_makers : page_maker list ref =
  ref []

let register_page_maker detect retrieve =
  Ocsigen_messages.errlog "register page maker";
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

let url_of id _ =
  preapply HTTP_services.page_of (identifier_to_string_list id)

let creation_page lid sid creation_service =
  let goto_page_of _ = preapply HTTP_services.page_of lid in
  let yes_service = creation_service goto_page_of goto_page_of in
  div ~a:[a_class ["message_box"]] I18N.String.([
    p [ pcdata (does_not_exist sid) ];
    p [ pcdata do_you_want_to_create_it ];
    menu_button yes_service yes lid;
    menu_button HTTP_services.root no ();
  ])

let error_page msg =
  div ~a:[a_class ["message_box"]] I18N.String.([
    p [ pcdata msg ]
  ])

let offer_creation emake creation_service page id =
  let lid = identifier_to_string_list id in
  let sid = string_of_identifier id in
  emake id >>= function
    | `OK e -> return (page e)
    | `KO (`UndefinedEntity e) -> return (creation_page lid sid creation_service)
    | `KO e -> return (error_page (CORE_error_messages.string_of_error e))

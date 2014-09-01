(* -*- tuareg -*- *)

(** Entities HTML pages indexed by entities identifiers. *)
{shared{
open Lwt
open Eliom_content.Html5
open Eliom_content.Html5.D
open Html5_types
open Eliom_service
}}

open ExtPervasives
open WidgetHTML
open HTML
open Identifier

{shared{
type editor_maker =
    (unit -> EditorHTML.interface) client_value
}}

type page_descriptor =
  (editor_maker -> [ div_content ] elt)
* (editor_maker -> [ body_content ] elt)

type page_maker =
    (identifier -> bool) * (identifier -> page_descriptor Lwt.t)

let page_makers : page_maker list ref =
  ref []

let register_page_maker detect retrieve =
  page_makers := (detect, retrieve) :: !page_makers

let empty_page =
  ((fun _ -> div []),
   (fun _ -> div []))

let get_page id =
  try_lwt
    let (_, retrieve) =
      List.find (fun (detect, _) -> detect id) !page_makers
    in
    retrieve id
  with Not_found ->
    return empty_page

let () =
  HTML.Hackojo_app.register
    ~secure_session:true
    ~service:ServicesHTML.page_of
    (fun id () ->
      let id = make (List.map label id) in
      lwt links, body = get_page (identifier_of_path id) in
      HTML.hackojo_page links body
    )

let url_of id _ =
  preapply ServicesHTML.page_of (string_list_of_identifier id)

let creation_page lid sid creation_service =
  let denied = div [pcdata I18N.String.access_denied] in
  UserHTTP.(
    teacher_only () >>>= fun u ->
    let goto_page_of _ = preapply ServicesHTML.page_of lid in
    let yes_service = creation_service u goto_page_of goto_page_of in
    return (`OK (ServicesHTML.(
      div ~a:[a_class ["message_box"]] I18N.(String.([
        p [ pcdata (does_not_exist sid) ];
        p [ pcdata do_you_want_to_create_it ];
        menu_button ~xa:[a_class ["left_side"]] yes_service (cap yes) lid;
        menu_button ~xa:[a_class ["right_side"]] root (cap no) ()
      ])))))
  ) >>= function
    | `OK e -> return ((fun _ -> div []), fun _ -> e)
    | `KO _ -> return ((fun _ -> div []), fun _ -> denied)

let error_page msg =
  (fun _ -> div []),
  fun _ ->
  div ~a:[a_class ["message_box"]] I18N.String.([
    p [ pcdata msg ]
  ])

let offer_creation emake creation_service page id =
  let lid = string_list_of_identifier id in
  let sid = string_of_identifier id in
  emake id >>= function
    | `OK e ->
      page e
    | `KO (`UndefinedEntity e) ->
      creation_page lid sid creation_service
    | `KO e ->
      return (error_page "error") (* FIXME: Display a nice error msg. *)

{shared{
let get_progress () =
  HTML.get_img
    ~a:[a_id "loader"; a_class ["inlined"]]
    ~alt:"loading" "ajax-loader.gif"
}}

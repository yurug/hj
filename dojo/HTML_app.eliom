(* -*- tuareg -*- *)

(** The Hackojo web application. *)

(** This  module register  the web application  in Eliom  and provides
    common definitions  shared between all  the HTML pages of  the web
    interface. *)

open Lwt
open Eliom_content.Html5.D
open Eliom_content.Html5.Id
open Eliom_lib

module Hackojo_app =
  Eliom_registration.App (
    struct
      let application_name = "hackojo"
    end)

let get_img fname =
  img
    ~a:[a_id "logo"]
    ~src:(make_uri ~service:(Eliom_service.static_dir ()) ["img"; fname])
    ~alt:("logo")
    ()

let logo =
  get_img "logo.png"

let bar =
  div ~a:[a_id "bar"] [
    logo;
  ]

type div = [ Html5_types.body_content_fun ] Eliom_content.Html5.D.elt

type hackojo_scroll = {
  elt      : div;
  subs     : div;
  commands : div;
}

let elt_of_hackojo_scroll x = x.elt

let hackojo_scroll status short_description description commands =
  let subs = div ~a:[a_class ["scroll_item_subs"]] [] in
  let description =
    div ~a:[ a_class [ "scroll_description" ]] [ description ]
  in
  let (expand_button, description) = HTML_widget.show_or_hide description in
  let commands =
    div ~a:[ a_class [ "scroll_commands" ]] (expand_button :: commands)
  in
  let elt = div ~a:[ a_class [ "scroll_entry" ]] [
        div ~a:[ a_class [ "scroll_status"; "scroll_side" ]] [ status ];
        div ~a:[ a_class [ "scroll_item" ]] [
          div ~a:[ a_class [ "scroll_item_main" ]] [
            div ~a:[ a_class [ "scroll_short_description" ]] [ short_description ];
            commands;
            description
          ];
          subs
        ]
      ]
  in

  let scroll = {
    elt = (elt :> div);
    subs = subs;
    commands = commands;
  }
  in
  scroll

let hackojo_page body_contents =
  Eliom_tools.F.html
    ~title:I18N.String.the_hacking_dojo
    ~css:[["css";"hackojo.css"]]
    (body ~a:[a_id "global"]
       (bar
        :: [
          div ~a:[a_id "bar_space"] [];
          div ~a:[a_id "contents"] body_contents
        ]))

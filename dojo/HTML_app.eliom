(* -*- tuareg -*- *)

(** The Hackojo web application. *)

(** This  module register  the web application  in Eliom  and provides
    common definitions  shared between all  the HTML pages of  the web
    interface. *)

open Lwt
open Eliom_content.Html5.D
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

type div = [ Html5_types.div ] Eliom_content.Html5.D.elt

type hackojo_scroll = {
  elt  : div;
  subs : div;
}

let div_of_hackojo_scroll x = x.elt

let hackojo_scroll status description commands =
  let subs = div ~a:[a_class ["scroll_item_subs"]] [] in
  {
    elt =
      div ~a:[ a_class [ "scroll_entry" ]] [
        div ~a:[ a_class [ "scroll_status"; "scroll_side" ]] [ status ];
        div ~a:[ a_class [ "scroll_item" ]] [
          div ~a:[ a_class [ "scroll_item_main" ]] [
            div ~a:[ a_class [ "scroll_description" ]] [ description ];
            div ~a:[ a_class [ "scroll_commands"; "scroll_side" ]] [  ]
          ];
          subs
        ]
      ];
    subs = subs
  }

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

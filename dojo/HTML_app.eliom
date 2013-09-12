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

let menu_button service label x =
  a ~a:[a_class ["menu_button"]] ~service [pcdata label] x

let about = menu_button HTTP_services.about I18N.(cap String.about) ()

let home = menu_button HTTP_services.root I18N.(cap String.home) ()

let user_menu =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    [ home; about ]

let menu () =
  lwt menu = Eliom_reference.get user_menu in
  return (div ~a:[a_id "menu"] (
    home :: menu @ [ about ])
  )

let set_menu buttons =
  Eliom_reference.set user_menu buttons

let bar () =
  lwt menu = menu () in
  return (
    div ~a:[a_id "bar"] [
      logo;
      menu
    ]
  )

let hackojo_page body_contents =
  lwt bar = bar () in
  return (
    Eliom_tools.F.html
      ~title:I18N.String.the_hacking_dojo
      ~css:[["css";"hackojo.css"]]
      (body ~a:[a_id "global"]
         (bar
          :: [
            div ~a:[a_id "bar_space"] [];
            div ~a:[a_id "contents"] body_contents
          ]))
  )

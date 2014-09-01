(* -*- tuareg -*- *)

(** The Hackojo web application. *)

(** This  module register  the web application  in Eliom  and provides
    common definitions  shared between all  the HTML pages of  the web
    interface. *)

{shared{
open Lwt
open Eliom_content.Html5.D
open Eliom_content.Html5.Id
open Eliom_lib
}}

module Hackojo_app =
  Eliom_registration.App (
    struct
      let application_name = "hackojo"
    end)

{shared{
let get_img ~a ~alt fname =
  img ~a ~alt
    ~src:(make_uri ~service:(Eliom_service.static_dir ()) ["img"; fname])
    ()
}}

let logo = get_img  ~alt:("logo") ~a:[a_id "logo"] "logo.png"

let menu_button ?(xa =[]) service label x =
  a ~a:(xa @ [a_class ["menu_button"]]) ~service [pcdata label] x

let menu_button_function ?(xa = []) cb label =
  span ~a:(
    xa @ [
      a_class ["menu_button"];
      a_onclick cb
    ]) [pcdata label]

let about = menu_button ServicesHTML.about I18N.(cap String.about) ()

let home = menu_button ServicesHTML.root I18N.(cap String.home) ()

let user_menu
: Html5_types.flow5 Eliom_content.Html5.D.elt list Eliom_reference.eref =
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

let default_menu () =
  set_menu []

let bar () =
  lwt menu = menu () in
  return (
    div ~a:[a_id "bar"] [
      logo;
      menu
    ]
  )

let include_javascript url =
  script
    ~a:[a_src url; a_mime_type "text/javascript"; a_charset "utf-8" ]
    (pcdata "")

let editor_css = Config.codemirror_editor_css ()

let editor_script = [
  include_javascript (Config.codemirror_editor_src ());
  include_javascript (Config.mathjax_src ())
]

let hackojo_page left_column central_column =
  lwt bar = bar () in
  let editor_interface = {unit -> EditorHTML.interface{ fun () ->
     %EditorHTML.make "editor_column"
  }}
  in
  return (
    Eliom_tools.F.html
      ~title:I18N.String.the_hacking_dojo
      ~css:[["css";"hackojo.css"]; editor_css]
      ~other_head:editor_script
      (body ~a:[a_id "global"]
         (bar
          :: [
            div ~a:[a_id "bar_space"] [];
            div ~a:[a_id "contents"] ([
              div ~a:[a_id "left_column"] [left_column editor_interface];
              div ~a:[a_id "central_column"] [central_column editor_interface];
              div ~a:[a_id "editor_column"] []
            ])
          ]))
  )

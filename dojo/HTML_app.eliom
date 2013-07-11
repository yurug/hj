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

let hackojo_page body_contents =
  Eliom_tools.F.html
    ~title:I18N.String.the_hacking_dojo
    ~css:[["css";"hackojo.css"]]
    (body ~a:[a_id "global"]
       (h1 [pcdata "Hackojo"]
        :: body_contents))

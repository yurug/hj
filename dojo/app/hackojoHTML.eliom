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

let root_service =
  Hackojo_app.register_service
    ~secure_session:true
    ~path:[""]
    ~get_params:Eliom_parameter.unit
    (fun () () ->
      return (html
                (head (title (pcdata "Hackojo")) [])
                (body [div [pcdata "Yo"]])
      )
    )

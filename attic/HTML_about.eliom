(* -*- tuareg -*- *)

(** This module defines the about page. *)
open Lwt
open Eliom_content
open Html5.D
open HTML_app

let about_page = div [ p [pcdata "About"] ]

let () =
  Hackojo_app.register
    ~secure_session:true
    ~service:HTTP_services.about
    (fun () () ->
      hackojo_page [about_page]
    )

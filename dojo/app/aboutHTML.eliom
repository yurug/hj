(* -*- tuareg -*- *)

(** This module defines the about page. *)
open Lwt
open Eliom_content
open Html5.D
open HTML

let about_page _ =
  return (div [ p [pcdata "About"] ])

let () =
  Hackojo_app.register
    ~secure_session:true
    ~service:ServicesHTML.about
    (fun () () ->
      hackojo_page (fun _ -> return (div [])) about_page
    )

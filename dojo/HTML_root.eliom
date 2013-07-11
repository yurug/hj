(* -*- tuareg -*- *)

(** Register the root service. *)

(** The root service serves an HTML page to let a user
    enter the dojo. *)

open Eliom_content.Html5.D
open Lwt
open HTML_app

let () =
  Hackojo_app.register
    ~service:HTTP_services.root
    (fun () () ->
(*    lwt homepage = HTML_user.connect_homepage HTML_services.root in *)
      return (hackojo_page [])
    )

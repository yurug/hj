(* -*- tuareg -*- *)

(** The Hackojo web application. *)

open Eliom_content.Html5.D
open Html5_types

(** This  module register  the Web application  in Eliom  and provides
    common definitions  shared between all  the HTML pages of  the web
    interface. *)

(** The Web application. *)
module Hackojo_app : Eliom_registration.ELIOM_APPL

(** [hackojo_page elts] produces an  HTML page containing  [elts] and
    everything  that  is   common  the  all  the  pages   of  the  web
    interface. *)
val hackojo_page : [ body_content_fun ] elt list -> html elt Lwt.t

(** [set_menu buttons] customizes the buttons in the menu bar. *)
val set_menu : [ div_content_fun ] elt list -> unit Lwt.t

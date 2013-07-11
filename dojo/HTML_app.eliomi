(* -*- tuareg -*- *)

(** The Hackojo web application. *)

(** This  module register  the Web application  in Eliom  and provides
    common definitions  shared between all  the HTML pages of  the web
    interface. *)

(** The Web application. *)
module Hackojo_app : Eliom_registration.ELIOM_APPL

(** [hackojo_page  elts] produces an  HTML page containing  [elts] and
    everything  that  is   common  the  all  the  pages   of  the  web
    interface. *)
val hackojo_page :
  [< Html5_types.body_content_fun > `H1 ] Eliom_content.Html5.D.elt list ->
  Html5_types.html Eliom_content.Html5.elt

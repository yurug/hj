(* -*- tuareg -*- *)

(** The Hackojo web application. *)

(** This  module register  the Web application  in Eliom  and provides
    common definitions  shared between all  the HTML pages of  the web
    interface. *)

(** The Web application. *)
module Hackojo_app : Eliom_registration.ELIOM_APPL

(** [hackojo_page elts] produces an  HTML page containing  [elts] and
    everything  that  is   common  the  all  the  pages   of  the  web
    interface. *)
val hackojo_page :
  [ Html5_types.body_content_fun ] Eliom_content.Html5.D.elt list ->
  Html5_types.html Eliom_content.Html5.elt

(** an [hackojo_scroll] is aimed to be the most common user interface
    widget of the system. It returns a scroll that is a hierarchical
    representation of something.

    By convention, each node of the tree contains four parts that
    corresponds to a (dynamically updated) status, a (static)
    description, a set of user commands that can be applied to the node
    and a sequence of sub-scrolls.
*)
type hackojo_scroll

(** [hackojo_scroll status description commands] creates a scroll
    with the given [status], [description] and [commands].
*)
val hackojo_scroll :
  [< Html5_types.div_content_fun ] Eliom_content.Html5.D.elt ->
  [< Html5_types.div_content_fun ] Eliom_content.Html5.D.elt ->
  [< Html5_types.div_content_fun ] Eliom_content.Html5.D.elt ->
  [ Html5_types.body_content_fun ] Eliom_content.Html5.D.elt list ->
  hackojo_scroll

val elt_of_hackojo_scroll
  : hackojo_scroll -> [ Html5_types.body_content_fun ] Eliom_content.Html5.D.elt

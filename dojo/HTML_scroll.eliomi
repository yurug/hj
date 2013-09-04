(* -*- tuareg -*- *)

(** The Swiss Army Knife widget. *)

(** an [hackojo_scroll] is aimed to be the most common user interface
    widget of the system. It returns a scroll that is a hierarchical
    representation of something.

    By convention, each node of the tree contains four parts that
    corresponds to a (dynamically updated) status, a (static)
    description, a set of user commands that can be applied to the node
    and a sequence of sub-scrolls.
*)
type hackojo_scroll

(** [hackojo_scroll status short_description ?start_shown description
    commands] creates a scroll with the given [status],
    [short_description], [description] and [commands]. [start_shown]
    is a flag to determine if the description is initially shown
    or not. (Default is [true].)
*)
val hackojo_scroll :
  [< Html5_types.div_content_fun ] Eliom_content.Html5.D.elt ->
  [< Html5_types.div_content_fun ] Eliom_content.Html5.D.elt ->
  ?start_shown:bool ->
  [< Html5_types.div_content_fun ] Eliom_content.Html5.D.elt ->
  [ Html5_types.body_content_fun ] Eliom_content.Html5.D.elt list ->
  hackojo_scroll

val elt_of_hackojo_scroll
  : hackojo_scroll -> [ Html5_types.body_content_fun ] Eliom_content.Html5.D.elt

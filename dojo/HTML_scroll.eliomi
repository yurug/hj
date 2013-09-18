(* -*- tuareg -*- *)

(** The Swiss Army Knife widget. *)

(** An [hackojo_scroll] is aimed to be the most common user interface
    widget of the system. It returns a scroll that is a hierarchical
    representation of something.

    By convention, each node of the tree contains four parts that
    corresponds to a (dynamically updated) status, a (static)
    description, a set of user commands that can be applied to the node
    and a sequence of sub-scrolls.
*)
type hackojo_scroll

type div = [ Html5_types.div ] Eliom_content.Html5.D.elt

(** [hackojo_scroll status short_description ?start_shown description
    commands] creates a scroll with the given [status],
    [short_description], [description] and [commands]. [start_shown]
    is a flag to determine if the description is initially shown
    or not. (Default is [true].)
*)
val hackojo_scroll :
  div ->
  div ->
  ?start_shown:bool ->
  ?description:div ->
  [ Html5_types.body_content_fun ] Eliom_content.Html5.D.elt list ->
  hackojo_scroll Lwt.t

val elt_of_hackojo_scroll : hackojo_scroll -> div

val push_subscrolls : hackojo_scroll list -> hackojo_scroll -> unit

type subs_request

val push : div -> subs_request

val from_server_to_subs : subs_request -> hackojo_scroll -> unit

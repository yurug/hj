(** -*- tuareg -*- *)

{shared{

(** HTML5 widgets. *)
open Dom_html
open Html5_types
open Eliom_content.Html5.D

(** The type of client-side mouse event handlers. *)
type onclick_cb = (unit -> unit) client_value

(** [button label onclick_cb] returns a button that executes
    [onclick_cb] when it is clicked. *)
val button: string list -> onclick_cb -> [> div ] elt

(** [menu_button label onclick_cb] returns a button that executes
    [onclick_cb] when it is clicked. *)
val small_button: string list -> onclick_cb -> [> div ] elt

(** [icon onclick_cb].  *)
val icon : [ flow5 ] elt list -> onclick_cb -> [> div ] elt

}}

(** [show_or_hide ?start_shown e] returns an element with a button to
    toggle its visibility. By default, [start_shown] is [true]. *)
val show_or_hide:
  ?start_shown:bool ->
  (unit, [ body_content_fun ] elt) server_function
  -> [ body_content_fun ] elt * [ body_content_fun ] elt

type editable_list = {
  fields    : string list;
  index_end : unit -> int Lwt.t;
  display   : int -> string list Lwt.t;
  remove    : (int -> string list -> unit Lwt.t) option;
  replace   : (int -> string list -> unit Lwt.t) option;
}

val list_editor
  :
  ?no_header:bool
  -> ?no_action:bool
  -> ?no_insertion:bool
  -> editable_list
  -> (int -> [ td_content_fun ] elt list)
  -> (int -> int -> [`RO | `RW ])
  -> [> div ] elt Lwt.t

val server_get_list_editor :
  ?no_header:bool
  -> ?no_action:bool
  -> ?no_insertion:bool
  -> string list
  -> (unit -> string list list Lwt.t)
  -> (string list list -> unit Lwt.t) option
  -> (int -> [ td_content_fun ] elt list)
  -> (int -> int -> [`RO | `RW ])
  -> [> div ] elt Lwt.t

val get_list_editor :
  ?no_header:bool
  -> ?no_action:bool
  -> ?no_insertion:bool
  -> string list
  -> (unit -> string list list Lwt.t)
  -> (string list list -> unit Lwt.t) option
  -> (int -> [ td_content_fun ] elt list)
  -> (int -> int -> [`RO | `RW ])
  -> (unit, [> div ] elt) server_function

val get_choices_editor :
  int list ->
  string list ->
  (int -> unit Lwt.t) ->
  (int -> unit Lwt.t) ->
  (unit, [> div ] elt) server_function

val nonempty_field : (string -> string option) client_value

val field :
  Html5_types.text ->
  [< string Eliom_parameter.setoneradio ] Eliom_parameter.param_name ->
  ?validator:(string -> string option) Eliom_pervasives.client_value ->
  ?fieldname:[ `Input ] Eliom_content.Html5.Id.id ->
  [< `Button | `Checkbox | `Color | `Date | `Datetime
  | `Datetime_local | `Email | `File | `Hidden | `Image
  | `Month | `Number | `Password | `Radio | `Range | `Reset | `Search
  | `Submit | `Tel | `Text | `Time | `Url | `Week ]
  -> string -> [> Html5_types.div ] Eliom_content.Html5.D.elt

type ('a, 'b, 'c) c =
    ?a:'a Eliom_content.Html5.D.attrib list ->
    'b Eliom_content.Html5.D.elt list -> 'c Eliom_content.Html5.D.elt

val fileuploader_wrapper :
  float -> float ->
  (string -> (string * (unit -> unit Lwt.t)) Lwt.t) ->
  ([> `Class | `Style_Attr ], [> Html5_types.input ] as 'a, Html5_types.form_content) c ->
  (unit -> unit) client_value ->
  'a Eliom_content.Html5.D.elt ->
    [> Html5_types.form ] Eliom_content.Html5.elt

val fileuploader :
  float -> float
  -> (string -> (string * (unit -> unit Lwt.t)) Lwt.t) -> [ td_content ] elt

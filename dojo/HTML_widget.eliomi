(* -*- tuareg -*- *)

{shared{

(** HTML5 widgets. *)
open Dom_html
open Html5_types
open Eliom_content.Html5.D

(** The type of client-side mouse event handlers. *)
type onclick_cb = (mouseEvent Js.t -> unit) client_value

(** [button label onclick_cb] returns a button that executes
    [onclick_cb] when it is clicked. *)
val button : string -> onclick_cb -> [> span ] elt

}}

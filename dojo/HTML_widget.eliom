(* -*- tuareg -*- *)

{shared{

open Eliom_content
open Html5
open Html5.D

type onclick_cb = (Dom_html.mouseEvent Js.t -> unit) client_value

let button label onclick =
  span ~a:[a_onclick onclick; a_class ["button"]] [pcdata label]

}}

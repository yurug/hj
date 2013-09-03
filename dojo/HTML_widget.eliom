(* -*- tuareg -*- *)

{shared{

open Eliom_content
open Html5
open Html5.D
open Html5_types

open COMMON_pervasives

type onclick_cb = (Dom_html.mouseEvent Js.t -> unit) client_value

let button label onclick =
   span ~a:[a_onclick onclick; a_class ["button"]] [pcdata label]

type show_state =
  | Hidden of string
  | Shown

}}

{client{
let toggle s e =
  match !s with
    | Shown ->
      s := Hidden (Html5.Manip.Css.display e);
      Manip.SetCss.display e "none"
    | Hidden d ->
      Manip.SetCss.display e d;
      s := Shown
}}

{shared{

let show_or_hide e =
  let see : [ body_content_fun ] elt =
    button (I18N.cap I18N.String.see) {{
      let s = ref Shown in
      fun _ -> toggle s %e
    }}
  in
     (see, e)

}}

(* -*- tuareg -*- *)

{shared{
open Eliom_content
open Html5
open Html5.D
open Html5_types

open COMMON_pervasives

type onclick_cb = (Dom_html.mouseEvent Js.t -> unit) client_value

let button labels onclick =
  let id = Id.new_elt_id ~global:true () in
  let onclick = {{
    let state = ref 0 in
    let next () =
      if !state = List.length %labels - 1 then state := 0 else incr state
    in
    let label () = List.nth %labels !state in
    fun e ->
      let open Eliom_content.Html5 in
      next ();
      Manip.Named.replaceAllChild %id [D.pcdata (label ())];
      %onclick e
   }}
  in
  let label = List.nth labels 0 in
   Id.create_named_elt ~id
     (span ~a:[a_onclick onclick; a_class ["button"]] [pcdata label])

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

let show_or_hide ?(start_shown=true) (e : [ body_content_fun ] elt) =
  let e = Id.create_global_elt e in
  let see : [ body_content_fun ] elt =
    let labels = [I18N.cap I18N.String.hide; I18N.cap I18N.String.see] in
    let labels = if start_shown then labels else List.rev labels in
    button labels {Dom_html.mouseEvent Js.t -> unit{
        let s = ref Shown in
        if not %start_shown then toggle s %e;
        fun (_ : Dom_html.mouseEvent Js.t) -> toggle s %e
      }}
  in
     (see, e)

}}

let field id name ?fieldname input_type text =
  let input_a = match fieldname with None -> [] | Some id -> [ a_id id ] in
  div ~a:[a_id id] [
    label ~a:[a_for name] [ pcdata text ];
    string_input ~a:input_a ~input_type ~name ()
  ]

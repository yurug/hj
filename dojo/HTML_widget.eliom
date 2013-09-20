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

let always_valid : (string -> string option) client_value =
  {{ fun (_ : string) -> (None : string option) }}

let nonempty_field : (string -> string option) client_value =
  {{ fun (s : string) ->
    if String.length s = 0 then
      Some I18N.String.this_field_must_not_be_empty
    else
      None
   }}

let validate_input validator id =
  match validator with
    | None ->
      ([], [])
    | Some validator ->
      let message = span [] in
      let validator =
        {{
          let open Eliom_content.Html5 in
              let nb = ref 0 in
              fun _ ->
                incr nb;
                let nb_now = !nb in
                let input_elt = Id.get_element %id in
                let input_value = (To_dom.of_input input_elt)##value in
                Lwt.async (fun () -> Lwt_js.sleep 0.5 >>
                  if !nb = nb_now then
                    let v =
                      match %validator (Js.to_string input_value) with
                        | None -> "✓ OK"
                        | Some reason -> "❌ " ^ reason
                    in
                    Lwt.return (
                      Manip.replaceAllChild %message [pcdata v]
                    ) else Lwt.return ()
                )
        }}
      in
      ([ a_oninput validator; a_onchange validator ], [message])

let field
    id name
    ?validator
    ?(fieldname : [ `Input ] Id.id option) input_type text =
  let input_id : [> `Input ] Id.id =
    match fieldname with
      | None -> Id.new_elt_id ~global:true ()
      | Some id -> (id :> [ `Input ] Id.id)
  in
  let input_validator, message =
    validate_input validator input_id
  in
  let input =
    Id.create_named_elt input_id  (
      string_input ~a:input_validator ~input_type ~name ()
    )
  in
  div ~a:[a_id id] ([
    label ~a:[a_for name] [ pcdata text ];
    (input :> [ div_content ] elt)
  ] @ message)

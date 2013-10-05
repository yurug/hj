(** -*- tuareg -*- *)

{shared{
open Lwt
open Eliom_content
open Eliom_parameter
open Html5
open Html5.D
open CORE_context
}}

let submit_file =
  Eliom_registration.Action.register_post_coservice'
    ~post_params:(Eliom_parameter.file "submission")
    (fun () f ->
      Ocsigen_messages.errlog "Received!";
      return ()
    )

{client{
let display_checkpoint exo_id ctx name =
  let prefix s = "▹ " ^ (I18N.String.answer_expected s) in
  let msg = match get_answer_form ctx with
    | Some (`Filename fname) -> prefix (I18N.String.in_a_file_named fname)
    | None -> ""
  in
(*  let submit id = fun evt ->
    let elt = Id.get_element id in
    let dom = To_dom.of_form elt in
    List.iter (fun (label, elt) ->
      match elt with
        | `File f -> Firebug.console##log_2 (label, elt);
          Lwt.async (Eliom_client.call_caml_service ~service:%submit_file f)
        | `String s -> Firebug.console##log_2 (label, s)
    ) (Form.form_elements dom)
  in *)
  let submit_form = match get_answer_form ctx with
    | Some (`Filename fname) ->
(*      let id = Id.new_elt_id () in
      let input = Html5.D.input ~input_type:`File ~a:[
          a_name ("submission_" ^ name);
          a_onchange (submit id)
        ] ()
      in
      let form =
        Id.create_named_elt ~id (get_form )
      in
      [form] *)
      [ post_form ~service:%submit_file (fun f -> [
        file_input ~name:f ();
        string_input ~input_type:`Submit ~value:"OK" ()
        ] ) ()
      ]
    | None -> []
  in
  return [div (
    p [pcdata msg]
    :: submit_form
  )]
}}

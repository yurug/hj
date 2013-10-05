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
  let submit_form = match get_answer_form ctx with
    | Some (`Filename fname) ->
      [ post_form ~xhr:false ~service:%submit_file (fun f -> [
        file_input ~name:f ();
        string_input ~input_type:`Submit ~value:"OK" ()
        ]) ()
      ]
    | None -> []
  in
  return [div (
    p [pcdata msg]
    :: submit_form
  )]
}}

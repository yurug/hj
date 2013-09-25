(** -*- tuareg -*- *)

{shared{
open Lwt
open Eliom_content
open Html5.D
}}

open HTTP_services
open HTML_app
open HTML_widget
open HTML_scroll
open CORE_exercise
open CORE_identifier
open I18N

{shared{
open CORE_client_reaction
open CORE_description_CST
}}

let exercise_page e =
  let id = identifier e in
  lwt init = raw_user_description_source e in
  lwt (editor_div, editor_id) =
    HTML_editor.create (CORE_source.content init)
      {{ fun echo (s : string) ->
        match CORE_description_format.questions_of_string s with
          | `OK cst ->
            echo "";
            return (Some cst)
          | `KO e ->
            echo (CORE_error_messages.string_of_error e);
            return None
       }}
      (server_function Json.t<questions with_raw> (fun cst ->
        change_from_user_description e cst
        >> return ()
       ))
  in
  let e_channel = CORE_entity.channel e in
  ignore {unit{ CORE_client_reaction.react_on_background %e_channel (
    fun data ->
      (* FIXME: in the future, we will try to "merge" the current state
         FIXME: of the editor. *)
      lwt content = CORE_exercise.raw_user_description %id in
      HTML_editor.refresh %editor_id content;
      Lwt.return ()
  )}};
  return editor_div

let exercise_page =
  HTML_entity.offer_creation CORE_exercise.make create_service exercise_page

let () =
  HTML_entity.register_page_maker
    (fun id -> CORE_identifier.(is_prefix (make [label "exercises"]) id))
    exercise_page

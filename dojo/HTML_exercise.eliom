(** -*- tuareg -*- *)

{shared{
open Lwt
open Eliom_content
open Html5.D

open CORE_description_CST
}}

open HTTP_services
open HTML_app
open HTML_widget
open HTML_scroll
open CORE_exercise
open CORE_identifier
open I18N

let exercise_page e =
  lwt init = raw_user_description e in
  lwt editor =
    HTML_editor.create
      init
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
  return editor

let exercise_page =
  HTML_entity.offer_creation CORE_exercise.make create_service exercise_page

let () =
  HTML_entity.register_page_maker
    (fun id -> CORE_identifier.(is_prefix (make [label "exercises"]) id))
    exercise_page

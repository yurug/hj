(** -*- tuareg -*- *)

open Lwt
open Eliom_content
open Html5.D

open HTTP_services
open HTML_app
open HTML_widget
open HTML_scroll
open CORE_exercise
open CORE_identifier
open I18N

let exercise_page e =
  let x = ref 0 in
  lwt editor =
    HTML_editor.create
      {{ fun (s : string) ->
        Firebug.console##log (s ^ string_of_int (! %x));
        Lwt.return (Some ())
       }}
      (server_function Json.t<unit> (fun () ->
        incr x;
        Ocsigen_messages.errlog ("Update" ^ string_of_int !x);
        return ())
      )
  in
  return editor

let exercise_page =
  HTML_entity.offer_creation CORE_exercise.make create_service exercise_page

let () =
  HTML_entity.register_page_maker
    (fun id -> CORE_identifier.(is_prefix (make [label "exercises"]) id))
    exercise_page

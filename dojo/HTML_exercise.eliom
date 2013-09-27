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
open COMMON_pervasives
open I18N

{shared{
open CORE_client_reaction
open CORE_description_CST
}}

let exercise_page e =

  let id = identifier e in
  lwt init = raw_user_description_source e in

  let patch_request (start, stop, what) =
    return (HTML_editor.patch start stop what)
  in

  let push_online_definition =
    let ods = Hashtbl.create 13 in
    let create id =
      (CORE_question.make_blank id
       >>>= fun e ->
       CORE_question.change_from_user_description e (Hashtbl.find ods id)
      ) >>= function
        | `OK _ ->
          return []
        | `KO e ->
          Ocsigen_messages.errlog "Error during question creation";
          return [ HTML_editor.message (CORE_error_messages.string_of_error e)]
    in
    fun (id, (user_description : question_definition)) ->
      let sid = string_of_identifier id in

      (** There is a user interface problem here: At some point,
          someone may refuse the creation of some entity X and, so,
          may answer 'no' to the following confirmation. But, what if,
          later on, he decides that he finally wants to create X?
          Currently, we will not propose the creation of X because of
          its first answer.  Maybe a solution would be to attach a
          timeout to negative answers. *)

      Hashtbl.replace ods id user_description;
      return [
        HTML_editor.confirm
          (I18N.String.do_you_really_want_to_create_a_question_named sid)
          (server_function Json.t<unit> (fun () -> create id))
      ]
  in
  lwt (editor_div, editor_id) =
    HTML_editor.create (CORE_source.content init)
      {{ fun echo (s : string) ->
        Firebug.console##log_2 ("Change into ", Js.string s);
        match CORE_description_format.questions_of_string s with
          | `OK cst ->
            echo "";
            return (Some cst)
          | `KO e ->
            echo (CORE_error_messages.string_of_error e);
            return None
       }}
      (server_function Json.t<questions with_raw> (fun cst ->
        change_from_user_description e cst >>= function
          | `OK (new_ods, patch) ->
            lwt rqs = Lwt_list.map_s push_online_definition new_ods in
            lwt ps = match patch with
              | None -> return []
              | Some p -> lwt r = patch_request p in return [r]
            in
            return (List.flatten rqs @ ps)
          | `KO e ->
            return [HTML_editor.message (CORE_error_messages.string_of_error e)]
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

(** -*- tuareg -*- *)

{shared{
open Lwt
open Eliom_content
open Html5.D
open Html5
open CORE_error_messages
}}

open HTTP_services
open HTML_app
open HTML_widget
open HTML_scroll
open CORE_evaluation
open CORE_exercise
open CORE_identifier
open CORE_entity
open CORE_error_messages
open COMMON_pervasives
open I18N

{shared{
open CORE_client_reaction
open CORE_description_CST
}}

let editor_div (e : CORE_exercise.t) =

  let id = CORE_exercise.identifier e in
  lwt init = raw_user_description_source e in

  let patch_request (start, stop, what) =
    return (HTML_editor.patch start stop what)
  in

  let push_online_definition =
    let ods = Hashtbl.create 13 in
    let create id =
      (CORE_exercise.make_blank id >>>= fun q ->
       CORE_exercise.push_dependency e "questions" [] (SomeEntity q)
       >> CORE_exercise.change_from_user_description q (Hashtbl.find ods id)
      ) >>= function
        | `OK _ ->
          return []
        | `KO (`NeedPatch p) ->
          lwt r = patch_request p in
          return [r]
        | `KO (#CORE_errors.all as e) ->
          return [HTML_editor.message (CORE_error_messages.string_of_error e)]
    in
    fun (id, user_description) ->
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
  let client_change =
    {{ fun echo (s : string) ->
      Firebug.console##log ("Client change!");
      match CORE_description_format.exercise_of_string s with
        | `OK cst ->
          echo "";
          return (Some cst)
        | `KO e ->
          echo (CORE_error_messages.string_of_error e);
          return None
     }}
  in
  let server_change =
    (server_function Json.t<exercise with_raw> (fun cst ->
      change_from_user_description e cst >>= function
        | `OK new_ods ->
          lwt rqs = Lwt_list.map_s push_online_definition new_ods in
          return (List.flatten rqs)
        | `KO (`NeedPatch p) ->
          lwt r = patch_request p in
          return [r]
        | `KO (#CORE_errors.all as e) ->
          return [HTML_editor.message (CORE_error_messages.string_of_error e)]
     ))
  in
  lwt (editor_div, editor_id, editor_process) =
    HTML_editor.create (CORE_source.content init) client_change server_change
  in
  (* FIXME: Optimize the following channel. For the moment, the entire
     description of the exercise is sent to the clients each time the
     exercise is modified.  This is HUGE! *)
  let e_channel = CORE_entity.channel e in
  ignore {unit{ CORE_client_reaction.react_on_background %e_channel (fun data ->
    lwt content = CORE_exercise.raw_user_description %id in
    match data with
    (* FIXME: In the future, we will try to "merge" the current state
       FIXME: of the editor. *)
    | CORE_entity.MayChange ->
      (** Oh, a question definition must have changed. *)
      begin match CORE_description_format.exercise_of_string content with
        | `OK cst ->
          lwt rqs = %server_change cst in
          Lwt_list.iter_s %editor_process rqs
        | `KO _ ->
          Firebug.console##log ("This should not happen.");
          return ()
      end

    | CORE_entity.HasChanged _ ->
      return (HTML_editor.refresh %editor_id content)
  )}};

  lwt source_div = HTML_source.entity_sources_div (module CORE_exercise) e in
  return (div [editor_div; source_div])

let exercise_div exo answer evaluation =
  let e_id = identifier exo in
  let display_context = server_function Json.t<checkpoint * CORE_context.t>
    (fun (cp, context) ->
      HTML_context.display_context e_id cp context evaluation
    )
  in
  let display_exercise =
    (* FIXME: For the moment, we redisplay the entire exercise
       FIXME: description each time it is updated. We should
       FIXME: check if this is reasonable or if we should
       FIXME: have finer notion of changes... *)
    {data -> [Html5_types.flow5] elt list Lwt.t{
      fun data ->
        match CORE_exercise.current_value data with
        | None -> return [p [pcdata "Displaying exercise..."]]
        | Some v ->
          lwt d = match v with
            | `KO e ->
              (* FIXME: For the moment, the error is display
                 in the exercise div. It may be more handy
                 to display it in the editor message bar.
                 (Yet, what if there is no editor because
                 the user is a student?) *)
              return [p [pcdata (string_of_error e)]]
            | `OK v ->
              let display_atomic = function
                | CORE_questions.Statement s ->
                  return [p [pcdata s]]
                | CORE_questions.CheckpointContext (cp, context) ->
                  %display_context (cp, context)
              in
              lwt all = Lwt_list.map_s display_atomic v in
              return (List.flatten all)
          in
          return (h1 [pcdata (CORE_exercise.title data)] :: d)
    }}
  in
  let get () = observe exo (fun d -> return d) in
  CORE_exercise.eval exo
  >> HTML_entity.reactive_div exo get display_exercise

type role =
  | Student   of CORE_user.t * CORE_user.t list
  | Evaluator of CORE_user.t
  | NoRole

let role e =
(* FIXME: Determine the role of the user wrt to this exercise.
   FIXME: This means determining the group of the user as well
   FIXME: as its status (student or evaluatuor.) *)
  CORE_user.authenticate "root" "foo" >>= function
    | `OK u -> return (Evaluator u)
    | `KO e -> warn e; return NoRole

let group_of_user e =
  role e >>= function
    | Evaluator u -> return [u]
    | Student (_, g) -> return g
    | NoRole -> return []

let exercise_page exo =
  (lwt group = group_of_user exo in
   CORE_answer.answer_of_exercise_from_authors exo group >>>= fun a ->
   CORE_evaluation.evaluation_of_exercise_from_authors exo a >>>= fun e ->
   (lwt r = role exo in
    (lwt_list_join (
      (match r with Evaluator _ -> [editor_div exo] | _ -> [])
      @ [exercise_div exo a e]
     ) >>= fun es -> return (`OK (div es)))))
  >>= function
    | `OK d -> return d
    | `KO e ->
      (* FIXME: Handle error properly. *)
      warn e;
      return (div [pcdata "Error when generating exercise page"])

let exercise_page =
  HTML_entity.offer_creation CORE_exercise.make create_service exercise_page

let () =
  HTML_entity.register_page_maker
    (fun id -> CORE_identifier.(is_prefix (make [label "exercises"]) id))
    exercise_page

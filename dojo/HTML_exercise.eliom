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
open CORE_inmemory_entity
open CORE_entity
open CORE_error_messages
open COMMON_pervasives
open I18N

{shared{
open CORE_client_reaction
open CORE_description_CST
}}

exception LoadingError

let editor_div (e : CORE_exercise.t) =

  let id = CORE_exercise.identifier e in
  lwt init = raw_user_description_source id >>= function
    | `OK s -> return s
    | `KO e -> raise_lwt LoadingError
  in

  let client_change =
    {{ fun echo (s : string) ->
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
      Ocsigen_messages.errlog "Change user description";
      change_from_user_description e cst
      >>= fun _ -> observe ~fresh:true e (fun d -> return d)
      >>= fun _ -> return []
     ))
  in
  lwt (editor_div, editor_id, editor_process) =
    HTML_editor.create (CORE_source.content init) client_change server_change
  in
  lwt sources_div =
    lwt d = HTML_source.entity_sources_div (module CORE_exercise) e in
    return (div ~a:[a_class ["exercise_sources"]] [ d ])
  in
  let editor_div = div ~a:[a_class ["exercise_editor"]] [editor_div] in
  return (div [sources_div; editor_div])

let editor_div e =
  try_lwt
    editor_div e
  with LoadingError -> return (div [pcdata "Error when loading editor."])

let exercise_div (exo : CORE_exercise.t) answer evaluation =
  let e_id = CORE_exercise.identifier exo in
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
                 FIXME: in the exercise div. It may be more handy
                 FIXME: to display it in the editor message bar.
                 FIXME: (Yet, what if there is no editor because
                 FIXME: the user is a student?) *)
              return [p [pcdata (string_of_error e)]]
            | `OK v ->
              let display_atomic = function
                | CORE_questions.Statement s ->
                  let d = div [] in
                  let d = To_dom.of_element d in
                  d##innerHTML <- Js.string s;
                  d##id <- Js.string "exercise_view";
                  return [(Of_dom.of_div d :> [Html5_types.flow5] elt)]
                | CORE_questions.CheckpointContext (cp, context) ->
                  %display_context (cp, context)
                | CORE_questions.Source _ ->
                  return []
              in
              Firebug.console##log (Js.string "Redisplay");
              lwt all = Lwt_list.map_s display_atomic v in
              return (List.flatten all)
          in
          return (h1 [pcdata (CORE_exercise.title data)] :: d)
    }}
  in
  let display_math = {{ fun () ->
    Js.Unsafe.eval_string
      "MathJax.Hub.Queue([\"Typeset\",MathJax.Hub, \"exercise_view\"]);";
   }}
  in
  let get () =
    CORE_exercise.eval_if_needed exo
    >>= fun _ ->
    CORE_exercise.observe ~fresh:true exo (fun d -> return (content d))
  in

  CORE_exercise.eval exo
  >>= fun _ -> (
    HTML_entity.reactive_div exo (Some display_math) get display_exercise
  )


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

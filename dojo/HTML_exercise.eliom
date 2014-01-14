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

{shared{
type editor_intermediate_result =
  | RCst of exercise with_raw
  | RStr of string
deriving (Json)
}}

let editor_div (e : CORE_exercise.t) authors =

  let id = CORE_exercise.identifier e in
  lwt init = raw_user_description_source id in

  let client_change =
    {{ fun echo (s : string) ->
      (* FIXME: Menhir produces mutually tail-recursive functions that
         FIXME: are not optimized by js_of_ocaml. So, if the source is
         FIXME: too large, the parsing is done on the server instead of
         FIXME: on the client. *)
      if String.length s < 4096 then
        match CORE_description_format.exercise_of_string s with
          | `OK cst ->
            echo "";
            return (Some (RCst cst))
          | `KO e ->
            echo (CORE_error_messages.string_of_error e);
            return None
      else
        return (Some (RStr s))
     }}
  in
  let server_change =
    (server_function Json.t<editor_intermediate_result> (
      let change cst =
        change_from_user_description e cst
        >>= fun _ -> observe e (fun d -> return d)
        >>= fun _ -> return []
      in
      function
      | RCst cst ->
        Ocsigen_messages.errlog "Change user description";
        change cst
      | RStr s ->
        match CORE_description_format.exercise_of_string s with
          | `OK cst ->
            change cst
          | `KO e ->
            let msg = CORE_error_messages.string_of_error e in
            return [HTML_editor.Message msg]
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

let editor_div e authors =
  try_lwt
    editor_div e authors
  with LoadingError -> return (div [pcdata "Error when loading editor."])

type role =
  | Student   of CORE_user.t * CORE_user.t list
  | Evaluator of CORE_user.t
  | NoRole

let exercise_div r (exo : CORE_exercise.t) answer evaluation authors =
  let e_id = CORE_exercise.identifier exo in
  let answer_id = CORE_answer.identifier answer in
  let display_context = server_function Json.t<checkpoint * CORE_context.t>
    (fun (cp, ctx) ->
      lwt ds = HTML_context.display_context e_id answer_id cp ctx evaluation in
      lwt ms =
        match r with
          | Evaluator master ->
            HTML_context.display_master_view master exo cp ctx
          | _ ->
            return []
      in
      return (ds @ ms)
    )
  in
  let elements = {string list ref{ ref [] }} in
  let display_exercise =
    (* FIXME: For the moment, we redisplay the entire exercise
       FIXME: description each time it is updated. We should
       FIXME: check if this is reasonable or if we should
       FIXME: have finer notion of changes... *)
    {CORE_questions.questions_result option * string
     -> [Html5_types.flow5] elt list Lwt.t{
      fun (current_value, title) ->
        match current_value with
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
              let display_atomic = CORE_questions.(function
                | Statement s ->
                  let d = div [] in
                  let d = To_dom.of_element d in
                  d##innerHTML <- Js.string s;
                  %elements := (Js.to_string d##id) :: !(%elements);
                  return [(Of_dom.of_div d :> [Html5_types.flow5] elt)]

                | CheckpointContext (cp, context) ->
                  %display_context (cp, context)
              )
              in
              Firebug.console##log (Js.string "Redisplay");
              lwt all = Lwt_list.map_s display_atomic v in
              return (List.flatten all)
          in
          return (h1 [pcdata title] :: d)
    }}
  in
  let display_math = {{ fun () ->
    Lwt.async (fun () ->
      return (Js.Unsafe.eval_string (
        Printf.sprintf
          "MathJax.Hub.Queue([\"Typeset\",MathJax.Hub,%s]);"
          (String.concat "," !(%elements))
      ))
    );
   }}
  in
  let get () =
    CORE_exercise.eval_if_needed exo authors >>= fun _ ->
    CORE_exercise.(observe exo (fun d ->
      let c = content d in
      return (current_value c, title c))
    )
  in

  CORE_exercise.eval exo authors >>= fun _ ->
  lwt rdiv =
    HTML_entity.reactive_div exo (Some display_math) get display_exercise
  in
    return (div ~a:[a_id "exercise"] [rdiv])

let role e =
(* FIXME: Determine the role of the user wrt to this exercise.
   FIXME: This means determining the group of the user as well
   FIXME: as its status (student or evaluatuor.) *)
  CORE_user.(logged_user () >>= function
    | `Logged u ->
      is_teacher u >>= (function
        | true -> return (Evaluator u)
        | false -> return (Student (u, [u])) (* FIXME *)
      )
    | _ -> return NoRole
  )

let group_of_user e =
  role e >>= function
    | Evaluator u -> return [u]
    | Student (_, g) -> return g
    | NoRole -> return []

let exercise_page exo =
  (lwt group = group_of_user exo in
   let gids = List.map CORE_user.identifier group in
   CORE_answer.answer_of_exercise_from_authors exo group >>>= fun a ->
   CORE_evaluation.evaluation_of_exercise_from_authors exo a gids >>>= fun e ->
   (lwt r = role exo in
    (lwt_list_join (
      (match r with Evaluator _ -> [editor_div exo gids] | _ -> [])
      @ [exercise_div r exo a e gids]
     ) >>= fun es -> return (`OK (div es)))))
  >>= function
    | `OK d ->
      return d
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

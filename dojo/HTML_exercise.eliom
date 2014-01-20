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
open I18N

{shared{
open CORE_client_reaction
open CORE_description_CST
open COMMON_pervasives
open COMMON_pervasives.LocalCache
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

{shared{

type display_command =
  | ShowPrelude of string
  | ShowValue of CORE_questions.atomic_value
  | ShowError of [ CORE_errors.all ]

deriving (Json)

}}

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
  let export_as_pdf = server_function Json.t<unit> (fun () ->
    CORE_exercise.export_as_pdf exo authors >>= function
      | Some filename -> return (Some (COMMON_file.send filename))
      | None -> return None
  )
  in
  let display_exercise =
    (* FIXME: For the moment, we redisplay the entire exercise
       FIXME: description each time it is updated. We should
       FIXME: check if this is reasonable or if we should
       FIXME: have finer notion of changes... *)
    {display_command
      -> [Html5_types.flow5] elt list Lwt.t{
        function
          | ShowPrelude title ->
            let download_as_pdf =
              HTML_widget.small_button [I18N.(String.(cap download_pdf))] (
                fun _ -> Lwt.async (fun () ->
                  %export_as_pdf () >>= function
                 | None ->
                   return ()
                 | Some url ->
                   return (
                     Dom_html.window##location##assign (Js.string url))
                )
              )
            in
            return [h1 [pcdata title]; download_as_pdf]

          | ShowValue a -> CORE_questions.(
            match a with
              | Statement s ->
                let d = div (HTML_statement.to_html s) in
                let dom = To_dom.of_div d in
                %elements := (Js.to_string dom##id) :: !(%elements);
                return [d]

              | CheckpointContext (cp, context) ->
                %display_context (cp, context)
          )

          | ShowError e ->
            return [div [pcdata (string_of_error e)]]
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
  let get =
    fun () ->
      CORE_exercise.eval_if_needed exo authors >>= fun _ ->
      CORE_exercise.(observe exo (fun d ->
        let c = content d in
        let cvs =
          match current_value c with
            | Some (`OK vs) -> List.map (fun v -> ShowValue v) vs
            | Some (`KO e) -> [ShowError e]
            | None -> []
        in
        return (ShowPrelude (title c) :: cvs)
      ))
  in

  CORE_exercise.eval exo authors >>= fun _ ->
  lwt rdiv =
    HTML_entity.reactive_div
      [CORE_entity.SomeEntity exo]
      (Some display_math) get display_exercise
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

let denied = return (`OK (div [pcdata I18N.String.access_denied]))

let assignment_rules_satisfied exo g =
  Lwt_list.for_all_s (fun u ->
    try_lwt
      lwt props = CORE_user.properties u in
      lwt can = CORE_exercise.assignment_rule exo `Can in
      return (CORE_property.evaluate props can)
    with _ -> return false
  ) g

let access_control exo proceed =
  role exo >>= function
    | Evaluator _ -> proceed ()
    | NoRole -> denied
    | Student (_, g) ->
      assignment_rules_satisfied exo g >>= function
        | true -> proceed ()
        | _ -> denied

let exercise_page exo =
  (lwt group = group_of_user exo in
   access_control exo (fun () ->
     let ids = List.map CORE_user.identifier group in
     CORE_answer.answer_of_exercise_from_authors exo group >>>= fun a ->
     CORE_evaluation.evaluation_of_exercise_from_authors exo a ids >>>= fun e ->
     (lwt r = role exo in
      (lwt_list_join (
        (match r with Evaluator _ -> [editor_div exo ids] | _ -> [])
        @ [exercise_div r exo a e ids]
       ) >>= fun es -> return (`OK (div es)))))
     >>= function
       | `OK d ->
         return d
       | `KO e ->
      (* FIXME: Handle error properly. *)
         warn e;
         return (div [pcdata "Error when generating exercise page"])
  )

let exercise_page id =
  let denied = return (div [pcdata I18N.String.access_denied]) in
  CORE_user.(logged_user () >>= function
    | `Logged u -> (
      is_teacher u >>= (function
        | false -> denied
        | true ->
          HTML_entity.offer_creation
            CORE_exercise.make create_service exercise_page id
      ))
    | _ -> denied
  )

let () =
  HTML_entity.register_page_maker
    (fun id -> CORE_identifier.(is_prefix (make [label "exercises"]) id))
    exercise_page

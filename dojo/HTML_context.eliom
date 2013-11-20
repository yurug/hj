(** -*- tuareg -*- *)

{shared{
open Lwt
open Eliom_content
open Eliom_parameter
open Ocsigen_extensions
open Html5
open Html5.D
open CORE_context
open CORE_answer
open CORE_exercise
open CORE_identifier
open CORE_error_messages
open COMMON_pervasives
}}

let display_score checkpoint (evaluation : CORE_evaluation.t) =
  let get () = CORE_evaluation.(
    lwt d = observe evaluation (fun d -> return d) in
    flush_diagnostic_commands_of_checkpoint evaluation checkpoint
    >> return d)
  in
  let diagnostic = Id.create_global_elt (div []) in
  lwt d =
    HTML_entity.reactive_div evaluation get {{
      let rec interpret_diagnostic_command = CORE_diagnostic.(function
        | Empty ->
          ()
        | PushLine s ->
          Eliom_content.Html5.Manip.appendChild %diagnostic (p [pcdata s])
        | Seq (c1, c2) ->
          interpret_diagnostic_command c1;
          interpret_diagnostic_command c2
      )
      in
      fun d ->
        CORE_evaluation.(
          match COMMON_pervasives.opt_assoc %checkpoint d.jobs with
            | Some Unevaluated ->
              return [pcdata "Pas évalué"]
            | Some (BeingEvaluated (_, dcmd, _)) ->
              interpret_diagnostic_command dcmd;
              return [pcdata "En cours..."]
            | Some (Evaluated (score, dcmd, _)) ->
              interpret_diagnostic_command dcmd;
              (* FIXME: Display the folded diagnostic. *)
              return [pcdata "Fini"]
            | None ->
              return [pcdata "?"]
        )
    }}
  in
  return (div [d; diagnostic])

let submit_file exo_id cp tmp_filename filename =
  CORE_user.authenticate "root" "foo" >>= function
    | `OK u -> (
      CORE_exercise.make exo_id >>>= fun exo ->
      answer_of_exercise_from_authors exo [u] >>= function
        | `OK a ->
          submit_file a cp tmp_filename filename
        | `KO e ->
          warn e;
          return (`OK ())
    )
    | `KO e ->
      warn e;
      return (`OK ())

let display_user_input exo_id checkpoint context =
  match CORE_context.get_answer_form context with
    | None -> return (p [pcdata "..."])
    | Some filename ->
      let tmp_filename = Filename.temp_file "hj" "" in
      let commit () =
        (* FIXME: handle error. *)
        submit_file exo_id checkpoint tmp_filename filename
        >> return ()
      in
      return (HTML_widget.fileuploader (fun user_filename ->
        (* FIXME: Check user_filename = filename. *)
        return (tmp_filename, commit)
      ))

let display_context exo_id checkpoint context evaluation =
  lwt user_input = display_user_input exo_id checkpoint context in
  lwt score = display_score checkpoint evaluation in
  return [
    user_input;
    score
  ]

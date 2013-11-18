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

let submit_file =
  Eliom_registration.Action.register_post_coservice'
    ~post_params:(Eliom_parameter.(
      string "id" ** (string "checkpoint" ** file "submission"))
    )
    (fun () (id, (name, file)) ->
      let exo_id = CORE_identifier.identifier_of_string id in

      Ocsigen_messages.errlog
        (Printf.sprintf "Received a file:\n %s\n%s\n%s"
           file.tmp_filename
           file.raw_original_filename
           file.original_basename);

      (CORE_user.authenticate "root" "foo" >>= function
        | `OK u -> (
          CORE_exercise.make exo_id >>>= fun exo ->
          answer_of_exercise_from_authors exo [u] >>= function
            | `OK a ->
              submit_file a name file.tmp_filename file.original_basename

            | `KO e ->
              warn e;
              return (`OK ())
        )
        | `KO e ->
          warn e;
          return (`OK ()))
      >>= function
        | _ -> (* FIXME *) return ()
    )

{client{

let display_checkpoint exo_id ctx name =
  let exo_id_s = CORE_identifier.string_of_identifier exo_id in
  let prefix s = "▹ " ^ (I18N.String.answer_expected s) in
  let msg = match get_answer_form ctx with
    | Some (`Filename fname) -> prefix (I18N.String.in_a_file_named fname)
    | None -> ""
  in
  let submit_form = match get_answer_form ctx with
    | Some (`Filename fname) ->
      [ post_form ~xhr:false ~service:%submit_file (fun (id, (cp, f)) -> [
        file_input ~name:f ();
        string_input ~input_type:`Hidden ~name:id ~value:exo_id_s ();
        string_input ~input_type:`Hidden ~name:cp ~value:name ();
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

let display_score checkpoint (evaluation : CORE_evaluation.t) =
  let get () = CORE_evaluation.observe evaluation (fun d -> return d) in
  let diagnostic = Id.create_global_elt (div []) in
  lwt d =
    HTML_entity.reactive_div evaluation get {{
      let interpret_diagnostic_command = CORE_diagnostic.(function
        | Reset ->
          Eliom_content.Html5.Manip.replaceAllChild %diagnostic []
        | PushLine s ->
          Eliom_content.Html5.Manip.appendChild %diagnostic (p [pcdata s])
      )
      in
      fun d ->
        CORE_evaluation.(
          match COMMON_pervasives.opt_assoc %checkpoint d.jobs with
            | Some Unevaluated ->
              return [pcdata "Pas évalué"]
            | Some (BeingEvaluated (_, dcmd)) ->
              interpret_diagnostic_command dcmd;
              return [pcdata "En cours..."]
            | Some (Evaluated score) ->
              (* FIXME: Display the folded diagnostic. *)
              return [pcdata "Fini"]
            | None ->
              return [pcdata "?"]
        )
    }}
  in
  return [d; diagnostic]

let display_context checkpoint context evaluation =
  return [p [pcdata "Context"]]

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
open CORE_inmemory_entity
open COMMON_pervasives
}}

let display_score checkpoint (evaluation : CORE_evaluation.t) =
  let get () = CORE_evaluation.(
    lwt d = observe ~who:(identifier_of_string "HTML_context") evaluation (fun d -> return (content d)) in
(*    flush_diagnostic_commands_of_checkpoint evaluation checkpoint
    >> *) return d)
  in
  let diagnostic = div [] in
  lwt d =
    HTML_entity.reactive_div evaluation None get {{
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
              return [p [pcdata "▹ Pas évalué"]]
            | Some (BeingEvaluated (_, _, dcmd, _)) ->
              interpret_diagnostic_command dcmd;
              return [p [pcdata "▹ En cours..."]]
            | Some (Evaluated (score, _, dcmd, _)) ->
              interpret_diagnostic_command dcmd;
              (* FIXME: Display the folded diagnostic. *)
              return [p [pcdata ("▹ " ^ string_of_score score)]]
            | None ->
              return [p [pcdata "?"]]
        )
    }}
  in
  return (div [d; diagnostic])

(* FIXME: Factorize the following three functions. *)

let submit_file exo_id cp tmp_filename filename =
  CORE_user.logged_user () >>= function
    | `Logged u -> (
      CORE_exercise.make exo_id >>>= fun exo ->
      answer_of_exercise_from_authors exo [u] >>= function
        | `OK a ->
          CORE_answer.submit_file a cp tmp_filename filename
        | `KO e ->
          warn e;
          return (`OK ())
    )
    | _ ->
      return (`OK ())

let submit_answer_choices exo_id cp vs =
  CORE_user.logged_user () >>= function
    | `Logged u -> (
      CORE_exercise.make exo_id >>>= fun exo ->
      answer_of_exercise_from_authors exo [u] >>= function
        | `OK a ->
          CORE_answer.submit_answer_choices a cp vs
        | `KO e ->
          warn e;
          return (`OK ())
    )
    | _ ->
      return (`OK ())

let submit_answer_values exo_id cp vs =
  CORE_user.logged_user () >>= function
    | `Logged u -> (
      CORE_exercise.make exo_id >>>= fun exo ->
      answer_of_exercise_from_authors exo [u] >>= function
        | `OK a ->
          CORE_answer.submit_answer_values a cp vs
        | `KO e ->
          warn e;
          return (`OK ())
    )
    | _ ->
      return (`OK ())

let submit_property_choice exo_id cp vs =
  CORE_user.logged_user () >>= function
    | `Logged u -> (
      CORE_exercise.make exo_id >>>= fun exo ->
      answer_of_exercise_from_authors exo [u] >>= function
        | `OK a ->
          CORE_answer.submit_property_choice a cp vs
        | `KO e ->
          warn e;
          return (`OK ())
    )
    | _ ->
      return (`OK ())

let display_user_input exo_id answer_id checkpoint context submission =
  match CORE_context.get_answer_form context with
    | None ->
      return (p [pcdata "..."])

    | Some (`Filename filename) ->
      let tmp_filename = Filename.temp_file "hj" "" in
      let commit () =
        (* FIXME: handle error. *)
        submit_file exo_id checkpoint tmp_filename filename
        >>= fun _ -> return ()
      in
      let previous_file =
        match submission with
          | Some (SubmittedFile (filename, digest)) ->
            [
              span [pcdata (I18N.(String.(cap last_submitted_file) ^ ": "))];
              Raw.a ~a:[a_href (Xml.uri_of_string (COMMON_file.send (
                CORE_standard_identifiers.source_filename answer_id filename
              )))] [pcdata (filename ^ "(" ^ Digest.to_hex digest ^ ")")]
            ]
          | None ->
            []
      in
      let msg = filename ^ I18N.String.to_be_provided in
      let width = float_of_int (String.length msg) in
      let style = Printf.sprintf "height:1em; width:%fem;" width in

      return (div (
        (div ~a:[a_style style; a_class ["user_input_file"]] [
          HTML_widget.fileuploader_wrapper width 1. (fun user_filename ->
          (* FIXME: Check user_filename = filename. *)
            return (tmp_filename, commit)
          ) span (pcdata msg)
        ]
        ) :: previous_file
      ))

    | Some (`Choices cs) ->
      let initial_choices =
        match submission with
          | Some (SubmittedChoices cs) -> cs
          | _ -> []
      in
      let choices = ref initial_choices in
      let add x = return (choices := x :: !choices) in
      let del x = return (choices := List.filter (( = ) x) !choices) in
      let choices_editor =
        HTML_widget.get_choices_editor initial_choices cs add del
      in
      (* FIXME: The following sequence of code is too inelegant! *)
      let choices_div = div [] in
      {unit Lwt.t{
        lwt e = %choices_editor () in
        return (Manip.replaceAllChild %choices_div [e])
      }};
      let submit = server_function Json.t<unit> (fun () ->
        Ocsigen_messages.errlog "OK clicked.";
        submit_answer_choices exo_id checkpoint !choices
      )
      in
      let submit_button = HTML_widget.small_button ["OK"] {{
        fun _ ->
          Lwt.async (fun () -> %submit ())
      }}
      in
      return (div ~a:[a_class ["user_answer"]] [choices_div; submit_button])

    | Some (`KeyValues ks) ->
      let vs =
        match submission with
          | Some (SubmittedValues vs) -> vs
          | None -> List.map (fun _ -> "") ks
      in
      let answers = ref (List.map2 (fun k v -> [k; v]) ks vs) in
      let get () = return !answers in
      let set ss = return (answers := ss) in
      let fields = ["Key"; "Value"] in
      let extra _ = [] in
      let list_editor =
        HTML_widget.get_list_editor
          ~no_header:true
          ~no_insertion:true
          ~no_action:true
          fields get (Some set) extra
          (fun _ -> function 1 -> `RW | _ -> `RO)
      in
      (* FIXME: The following sequence of code is too inelegant! *)
      let editor_div = div [] in
      {unit Lwt.t{
        lwt e = %list_editor () in
        return (Manip.replaceAllChild %editor_div [e])
      }};
      let submit = server_function Json.t<unit> (fun () ->
        submit_answer_values exo_id checkpoint (
          List.map (function [_;x] -> x | l ->
            assert false) !answers
        )
      )
      in
      let submit_button = HTML_widget.small_button ["OK"] {{
        fun _ -> Lwt.async (fun () -> %submit ())
      }}
      in
      return (div ~a:[a_class ["user_answer"]] [editor_div;
                                                p [pcdata ""];
                                                submit_button])

    | Some (`ChooseProperty cs) ->
      let previous =
        match submission with
          | Some (SubmittedPropertyChoice s) -> s
          | None -> ""
      in
      let submit = server_function Json.t<string> (fun choice ->
        submit_property_choice exo_id checkpoint choice
      )
      in
      let id = Id.new_elt_id () in
      let select = {{ fun _ ->
        let e = Id.get_element %id in
        let e = To_dom.of_select e in
        Firebug.console##log (e##value);
        Lwt.async (fun () -> %submit (Js.to_string e##value))
      }}
      in
      let property_selector =
        Id.create_named_elt ~id (
          Raw.select ~a:[a_onchange select] (
            List.map (fun s ->
              let a = if s = previous then [a_selected `Selected] else [] in
              Raw.option ~a (pcdata s)) cs
          )
        )
      in
      return (div ~a:[a_class ["user_answer"]] [property_selector])


let extract_previous_submission checkpoint evaluation =
  CORE_evaluation.(observe evaluation (fun s ->
    let d = content s in
    match COMMON_pervasives.opt_assoc checkpoint d.jobs with
      | Some Unevaluated | None ->
        return None
      | Some (BeingEvaluated (_, s, _, _))
      | Some (Evaluated (_, s, _, _)) ->
        return (Some s)
  )
  )

let display_context exo_id answer_id checkpoint context evaluation =
  lwt submission = extract_previous_submission checkpoint evaluation in
  lwt user_input =
    display_user_input exo_id answer_id checkpoint context submission
  in
  lwt score = display_score checkpoint evaluation in
  return [
    user_input;
    div [ score ]
  ]

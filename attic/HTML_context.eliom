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

let update_if_necessary =
  let relaunched = Hashtbl.create 13 in
  fun answer_id evaluation_id checkpoint (d, s) ->
    let now = Unix.gettimeofday () in
    let reset_evaluation () =
      match evaluation_id with
        | Some id -> (CORE_evaluation.make id >>= function
            | `OK e -> CORE_evaluation.reset e checkpoint
            | `KO e -> warn e; return ()
        )
        | None -> return ()
    in
    let relaunch answer =
      Hashtbl.replace relaunched (answer_id, checkpoint) now;
      reset_evaluation ()
      >> CORE_answer.submit answer checkpoint s
    in
    (* FIXME: Make this literal a parameter! *)
    let old = 180. in
    if now -. d > old then
      CORE_answer.make answer_id >>= function
        | `OK answer -> (
          try_lwt
            let time = Hashtbl.find relaunched (answer_id, checkpoint) in
            if now -. time > old then relaunch answer else return ()
          with Not_found -> relaunch answer
        )
        | `KO e -> warn e; (* FIXME: handle error. *) return ()
    else
      return ()

let update_if_necessary_rpc answer evaluation checkpoint =
  server_function Json.t<float * submission> (
    update_if_necessary answer evaluation checkpoint
  )

let display_score answer_id checkpoint context evaluation =
  let get () =
    CORE_evaluation.observe evaluation (fun d -> return [content d])
  in
  let condition, trigger =
    (** If the context contains master grade, it must always be active. *)
    match CORE_context.get_master_grade context with
      | Some _ ->
        None, None
      | None ->
        None, None
  in
  let evaluation_id = CORE_evaluation.identifier evaluation in
  let update_if_necessary =
    update_if_necessary_rpc answer_id (Some evaluation_id) checkpoint
  in
  let diagnostic = div [] in
  lwt d =
    HTML_entity.reactive_div
      [CORE_entity.SomeEntity evaluation] None get {CORE_evaluation.description -> _ {
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
        let cs = %checkpoint ^ " " in
        CORE_evaluation.(
          match COMMON_pervasives.opt_assoc %checkpoint d.jobs with
            | Some Unevaluated ->
              return [p [pcdata (cs ^ "▹ Pas évalué")]]
            | Some (BeingEvaluated (_, date, submission, dcmd, _)) ->
              interpret_diagnostic_command dcmd;
              %update_if_necessary (date, submission)
              >> return [p [pcdata (cs ^ "▹ En cours...")]]
            | Some (Evaluated (score, _, dcmd, ctx)) ->
              Eliom_content.Html5.Manip.replaceChildren %diagnostic [];
              Firebug.console##log (Js.string (CORE_diagnostic.string_of_command dcmd));
              interpret_diagnostic_command dcmd;
              (* FIXME: Display the folded diagnostic. *)
              return [p [pcdata (cs ^ "▹ " ^ string_of_score ctx score)]]
            | None ->
              return [p [pcdata "?"]]
        )
    }}
  in
  return (div [d; diagnostic], trigger)

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

let display_user_input exo_id answer_id checkpoint context submission trigger =
  let trigger = match trigger with
    | None -> {unit -> unit Lwt.t{ fun () -> return () }}
    | Some trigger -> trigger
  in
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
      lwt previous_file =
        match submission with
          | Some (SubmittedFile (filename, digest)) ->
            lwt url =
              COMMON_file.send (
                CORE_standard_identifiers.source_filename answer_id filename
              )
            in
            return [
              span [pcdata (I18N.(String.(cap last_submitted_file) ^ ": "))];
              Raw.a ~a:[a_href (Xml.uri_of_string url)] [
                pcdata (filename ^ "(" ^ Digest.to_hex digest ^ ")")
              ]
            ]
          | None ->
            return []
      in
      let msg = filename ^ I18N.String.to_be_provided in
      let width = float_of_int (String.length msg) in
      let style = Printf.sprintf "height:1em; width:%fem;" width in
      let onchange = {unit -> unit{fun () ->
        Lwt.async (fun () -> %trigger ())
      }}
      in
      return (div (
        (div ~a:[a_style style; a_class ["user_input_file"]] [
          HTML_widget.fileuploader_wrapper width 1. (fun user_filename ->
          (* FIXME: Check user_filename = filename. *)
            return (tmp_filename, commit)
          ) span onchange (pcdata msg)
        ]
        ) :: previous_file
      ))

    | Some (`Choices cs) ->
      let initial_choices =
        match submission with
          | Some (SubmittedChoices cs) -> cs
          | _ -> []
      in
      (* FIXME: Use a set... *)
      let choices = ref initial_choices in
      let add x =
        return (if not (List.mem x !choices) then choices := x :: !choices)
      in
      let del x =
        return (choices := List.filter (( <> ) x) !choices)
      in
      let choices_editor =
        HTML_widget.get_choices_editor initial_choices cs add del
      in
      (* FIXME: The following sequence of code is too inelegant! *)
      let choices_div = div [] in
      {unit Lwt.t{
        lwt e = %choices_editor () in
        return (Manip.replaceChildren %choices_div [e])
      }};
      let submit = server_function Json.t<unit> (fun () ->
        submit_answer_choices exo_id checkpoint !choices
      )
      in
      let submit_button = HTML_widget.small_button ["OK"] {{
        fun _ ->
          Lwt.async (fun () ->
             %submit ()
             >> %trigger ()
          )
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
        return (Manip.replaceChildren %editor_div [e])
      }};
      let submit = server_function Json.t<unit> (fun () ->
        submit_answer_values exo_id checkpoint (
          List.map (function [_;x] -> x | l ->
            assert false) !answers
        )
      )
      in
      let submit_button = HTML_widget.small_button ["OK"] {{
        fun _ -> Lwt.async (fun () -> %submit () >> %trigger ())
      }}
      in
      return (div ~a:[a_class ["user_answer"]] [
        editor_div;
        p [pcdata ""];
        submit_button
      ])

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
      let property_selector =
        Raw.select (
          List.map (fun s ->
            let a = if s = previous then [a_selected `Selected] else [] in
            Raw.option ~a (pcdata s)) cs
        )
      in
      {unit{
        let e = To_dom.of_select %property_selector in
        let select = fun _ ->
          Lwt.async (fun () -> %submit (Js.to_string e##value) >> %trigger ());
          Js._true
        in
        Dom_html.(ignore (addEventListener e Event.change (handler select) Js._true));
      }};
      return (div ~a:[a_class ["user_answer"]] [property_selector])


let extract_previous_submission checkpoint answer_id =
  CORE_answer.make answer_id >>= function
    | `KO _ -> return None
    | `OK a -> submission_of_checkpoint a checkpoint >>= function
        | None | Some NoSubmission -> return None
        | Some (Submission (_, s)) -> return (Some s)

let display_context exo_id answer_id checkpoint context evaluation =
  lwt submission = extract_previous_submission checkpoint answer_id in
  lwt score, trigger = display_score answer_id checkpoint context evaluation in
  lwt user_input =
    display_user_input exo_id answer_id checkpoint context submission trigger
  in
  return [div ~a:[a_class ["context"]] [
    user_input;
    div [ score ];
  ]]

(* FIXME: Some parts of the following function should be moved to
   FIXME: CORE_context.  *)
let display_master_view master exo checkpoint context =
  lwt all_answers = CORE_answer.answers_of_exercise exo in
  let get =
    fun () ->
      lwt all_answers = CORE_answer.answers_of_exercise exo in
      lwt all_answers =
        match CORE_context.get_master_focus context with
          | Some groups ->
            lwt master_groups =
              Lwt_list.filter_s
                (fun g -> CORE_user.has_property master (CORE_property.atom g))
                groups
            in
            let interest_master a =
              Lwt_list.exists_s
                (fun g -> CORE_user.has_property a (CORE_property.atom g))
                master_groups
            in
            lwt all_answers =
              Lwt_list.filter_s
                (fun (authors, _) ->
                  lwt authors = Lwt_list.fold_left_s (fun authors a ->
                    CORE_user.make a >>= function
                      | `OK a -> return (a :: authors)
                      | `KO _ -> return authors
                  ) [] authors
                  in
                  Lwt_list.exists_s interest_master authors)
                all_answers
            in
            lwt all_answers =
              CORE_user.(Lwt_list.(map_s (fun (authors, a) ->
                lwt surnames = fold_left_s (fun s u -> make u >>= function
                    | `OK a -> lwt sn = surname a in return (sn :: s)
                    | `KO _ -> return s
                ) [] authors
                in
                return (surnames, (authors, a))
              ) all_answers))
            in
            return (snd (List.(
              split (sort (fun (s1, _) (s2, _) -> Pervasives.compare s1 s2)
                       all_answers
              ))))

          | _ ->
            return all_answers
      in
      let evaluations = Hashtbl.create 13 in
      let answer_idx = ref (-1) in
      let files = ref [] in
      let relaunchers = ref [] in
      let links = Hashtbl.create 13 in
      let get_link i = try Some (Hashtbl.find links i) with Not_found -> None in
      let get_score_statistics, count_score = Hashtbl.(
        let table = create 13 in
        let get ctx =
          let e =
            fold (fun k s l -> [string_of_score ctx k; string_of_int s] :: l)
              table []
          in
          return (List.(rev (sort Pervasives.compare e)))
        in
        let count s = replace table s (succ (try find table s with _ -> 0)) in
        get, count
      ) in


      let display_answer master_grade (authors, answer_id) =
        CORE_answer.make answer_id >>= function
          | `KO _ -> return [] (* FIXME: handle error. *)
          | `OK answer ->
            incr answer_idx;
            lwt answer_descr, submission =
              CORE_answer.submission_of_checkpoint answer checkpoint
              >>= function
                | None | Some NoSubmission ->
                  return ("?", None)
                | Some (Submission (_, submission)) ->
                  lwt s =
                    match submission with
                      | SubmittedFile (f, digest) ->
                        let file =
                          CORE_standard_identifiers.source_filename answer_id f
                        in
                        lwt link = COMMON_file.send file in
                        files := file :: !files;
                        Hashtbl.add links !answer_idx link;
                        return (f ^ "(" ^ Digest.to_hex digest ^ ")")
                      | SubmittedValues vs ->
                        return (String.concat "," vs)
                      | SubmittedChoices vs ->
                        return (String.concat "," (List.map string_of_int vs))
                      | SubmittedPropertyChoice s ->
                        return s
                  in
                  return (s, Some submission)
            in
            lwt evaluation, evaluation_id =
              CORE_evaluation.evaluation_of_exercise_from_authors
                exo answer authors
              >>= function
                | `KO _ ->
                  return (None, None)
                | `OK evaluation ->
                  let evaluation_id = CORE_evaluation.identifier evaluation in
                  return (Some evaluation, Some evaluation_id)
            in
            let master_score = ref None in

            let relaunch () =
              match submission with
                | None ->
                  return ()
                | Some s ->
                  update_if_necessary answer_id evaluation_id checkpoint (0., s)
            in
            relaunchers := relaunch :: !relaunchers;

            lwt evaluation_descr = CORE_evaluation.(
              (* FIXME: Do that more elegantly! *)
              match evaluation with
                | None ->
                  relaunch () >> return "error"
                | Some evaluation ->
                  state_of_checkpoint evaluation checkpoint >>= function
                    | None | Some Unevaluated ->
                      (match master_grade with
                        | Some (_, over) ->
                          master_score := Some ("?/" ^ string_of_int over)
                        | _ -> ());
                      relaunch () >> return "?"
                    | Some (Evaluated (s, _, _, ctx)) ->
                      let s = match master_grade with
                        | None -> s
                        | Some (criteria, over) ->
                          master_score := Some (
                           match CORE_context.grade_for_criteria criteria s with
                              | None ->
                                ("?/" ^ string_of_int over)
                              | Some (g, _) ->
                                (string_of_int g ^ "/" ^ string_of_int over)
                          );
                          CORE_context.except criteria s
                      in
                      count_score s;
                      return (CORE_context.string_of_score ctx s)
                    | Some (BeingEvaluated (_, d, s, _, _)) ->
                      update_if_necessary answer_id evaluation_id checkpoint
                        (d, s)
                      >> return ("En cours depuis " ^ string_of_date d)
            )
            in
            let display author =
              CORE_user.make author >>= function
                | `OK u ->
                  lwt firstname = CORE_user.firstname u in
                  lwt surname = CORE_user.surname u in
                  let cells =
                    [ firstname; surname; answer_descr; evaluation_descr ] @ (
                      match !master_score with
                        | None -> []
                        | Some s -> [s]
                    )
                  in
                  begin match evaluation with
                    | None -> ()
                    | Some evaluation ->
                      Hashtbl.add evaluations (firstname, surname) evaluation
                  end;
                  return cells
                | _ ->
                  return []
            in
            Lwt_list.map_s display authors
      in
      let master_grade = CORE_context.get_master_grade context in
      lwt list = Lwt_list.map_s (display_answer master_grade) all_answers in
      let list = List.flatten list in
      let header =
        I18N.(String.(List.map cap [firstname; surname; answer; score]))
        @ (match master_grade with
          | None -> []
          | Some (criteria, _) -> [criteria]
        )
      in
      let replace l =
        match master_grade with
          | None -> return ()
          | Some (criteria, _) ->
            Lwt_list.iter_s (function
              | [name; surname; _; _; mgrade] ->
                begin
                  try_lwt
                    let evaluation = Hashtbl.find evaluations (name, surname) in
                    let grade = ref 0 in
                    let over = ref 0 in
                    (* FIXME: Do more flexible parsing! *)
                    Scanf.sscanf mgrade "%d/%d" (fun g o ->
                      grade := g;
                      over := o
                    );
                    CORE_evaluation.new_score criteria
                      !grade !over evaluation checkpoint
                  with _ -> (* FIXME *)
                    return ()
                end
              | _ -> (* FIXME: Should not happen. *)
                return ()) l
      in
      lwt e = HTML_widget.server_get_list_editor
        ~no_insertion:true
        header
        (fun () -> return list) (* FIXME: should be dynamic. *)
        (Some replace)
        (fun i ->
          match get_link i with
            | None -> []
            | Some url -> [
              HTML_widget.icon [pcdata "↓"] {{ fun _ -> Lwt.async (fun () ->
                return (Dom_html.window##location##assign (Js.string %url))
              )}}]
        )
        (fun _ -> function 4 -> `RW | _ -> `RO)
      in
      lwt statistics =
        HTML_widget.server_get_list_editor
          ~no_insertion:true ~no_action:true ~no_header:true
          I18N.(String.([cap score; cap total]))
          (fun () -> get_score_statistics context)
          None
          (fun _ -> [])
          (fun _ _ -> `RO)
      in

      let relaunch_all_evaluation =
        let relaunch = server_function Json.t<unit> (fun () ->
          Lwt_list.iter_p (fun f -> f ()) !relaunchers
        )
        in
        HTML_widget.small_button [I18N.(String.(cap relaunch_all))] {{
          fun _ -> Lwt.async %relaunch
        }}
      in
      let download_all_files =
        let all_files = server_function Json.t<unit> (fun () ->
          if !files = [] then
            return None
          else
            let archive = Filename.temp_file "hjarc" ".tar.gz" in
            ltry (COMMON_unix.tar_create archive !files)
                      >>= function
                        | `KO e ->
                          warn e; return None
                        | `OK _ ->
                          lwt u = COMMON_file.send archive in return (Some u)
        )
        in
        HTML_widget.small_button [I18N.(String.(cap download_all))] {{
          fun _ -> Lwt.async (fun () -> %all_files () >>= function
            | None ->
              return ()
            | Some u ->
              return (Dom_html.window##location##assign (Js.string u))
          )}}
      in
      return [
        e :: [p [pcdata ""]; statistics; p [pcdata ""]] @ (
          if !files = [] then [] else [
            div [ download_all_files; relaunch_all_evaluation ]
          ])
      ]
  in
  let rdiv = server_function Json.t<unit> (fun () ->
    lwt answers =
      Lwt_list.fold_left_s (fun all (authors, aid) ->
        CORE_answer.make aid >>= function
          | `OK answer ->
            lwt evaluation = CORE_evaluation.(
              evaluation_of_exercise_from_authors exo answer authors
              >>= function
                | `OK e -> return [CORE_entity.SomeEntity e]
                | `KO _ -> return []
            )
            in
            return CORE_entity.(SomeEntity answer :: evaluation @ all)
          | `KO _ -> return all
      ) [] all_answers
    in
    let ws = (CORE_entity.SomeEntity exo) :: answers in
    HTML_entity.reactive_div ws None get {_ -> _ Lwt.t{ fun d -> return d }}
  )
  in
  let (see, div) = HTML_widget.show_or_hide rdiv in
  return [see; div]

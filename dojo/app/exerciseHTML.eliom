(* -*- tuareg -*- *)

(* FIXME: This is a huge module that should be split! *)

{shared{
open Lwt
open Eliom_content.Html5
open Eliom_content.Html5.D
open Html5_types
open Eliom_service
}}

open Identifier
open Questions
open Statement
open WidgetHTML
open ExerciseHTTP
open ExtPervasives
open StatementHTML

let fresh_id =
  let r = ref 0 in
  fun () -> incr r; "i" ^ string_of_int !r

let exercise_page exo =

  let focus = {string option ref{ ref None }} in
  let reset = {(unit -> unit) ref{ref (fun () -> ()) }} in

  let exo_id = Exercise.identifier exo in
  let exo_str = Identifier.string_of_identifier exo_id in

  let statement_div = div [] in

  let statement_as_html
      name title
      tags difficulty
      statements context answers editor_maker =
    let name_str : string = name in

    let codes = ref [] in

    let grade_div = div ~a:[a_class ["score_div"]] [] in

    let editor = {EditorHTML.interface option ref{ ref None }} in
    let get_editor = {unit -> EditorHTML.interface{ fun () -> match !(%editor) with
      | None -> raise Not_found
      | Some e -> e
    }}
    in

    let score_box = {string -> [p_content] elt list -> div elt{ fun score criteria ->
      div ~a:[a_class ["score_box"]] [
        p ~a:[a_class ["score"]] [pcdata score];
        p ~a:[a_class ["criteria"]] criteria
      ]
     }} in

    let display_evaluation_state =
      {([Html5_types.div_content_fun] elt list -> unit) option -> unit Lwt.t{
        fun console_write ->
        let open ExerciseHTTP in
        let criteria_as_html = function
          | Automatic -> pcdata "Dojo"
          | UserDefined s -> pcdata s
        in
        let scores_as_html scores =
            List.map (fun (c, (i, o)) ->
              %score_box (Printf.sprintf "%d/%d" i o) [criteria_as_html c]
            ) scores
        in
        let update_grade_div h =
          Manip.replaceChildren %grade_div h
        in
        let write_trace_on_console trace =
          match console_write with
            | None -> ()
            | Some write ->
              List.iter (function Message s -> write [p [pcdata s]]) trace
        in
        let process_command cmd =
          Js.Unsafe.eval_string cmd
        in
        let rec wait () = ExerciseHTTP.(
          %exercise_evaluation_state_server_function (%exo_str, %name_str)
          >>= function
            | EvaluationBeingProcessed ->
              update_grade_div [%score_box "..." []];
              Lwt_js.sleep 1. >> wait ()
            | EvaluationDone (_, _, _, grade, commands) ->
              write_trace_on_console grade.trace;
              List.iter process_command commands;
              return (update_grade_div (scores_as_html grade.scores))
            | EvaluationFailed ->
              return (update_grade_div [%score_box "!" []])
            | NoEvaluation ->
              return (update_grade_div [%score_box "?" []])
        )
        in
        wait ()
      }}
    in

    let qcm_as_html statements =

      let choices = {int list ref{ ref [] }} in

      let onload = {{
        fun _ -> !(%reset) ();
        %reset := (fun () -> ());
        %editor := None;
      }}
      in

      let oc (i : int) =
        {{ fun _ ->
          let i = %i + 1 in
          if not (List.mem i !(%choices)) then %choices := i :: !(%choices)
          else %choices := List.filter (fun j -> j <> i) !(%choices)
         }}
      in
      let qcm_item (i : int) statement =
        p [
          input ~input_type:`Checkbox ~a:[a_onclick (oc i)] ();
          span (template_text_as_html [] statement)
        ]
      in
      div ~a:[a_onload onload] (List.mapi qcm_item statements @ [
        small_button ["OK"] {unit -> unit{ fun () ->
          Lwt.async (fun () ->
            %push_new_choices_server_function (%exo_str, %name_str, !(%choices))
            >> (
              Manip.replaceChildren %grade_div [%score_box "..." []];
              Lwt_js.sleep 1.
            ) >> %display_evaluation_state None
          )
        }}
      ])
    in

    let grader_as_html expected_file =

      let expected_extension = Str.(
        if string_match (regexp ".*\\(\\..*\\)") expected_file 0 then
          matched_group 1 expected_file
        else
          ""
      )
      in
      lwt answer =
        try_lwt
          lwt a, author = Answers.answer_of_question answers name in
          return (Some (a, author))
        with Not_found ->
          return None
      in
      lwt answer_resource =
        let blank_resource = Resource.make expected_file "" in
        match answer with
          | Some (Questions.File a, author) ->
            (Answers.resource answers a >>= function
              | `OK (r, _) -> return r
              | `KO _ -> (Exercise.resource exo a >>= function
                  | `OK (r, _) -> return r
                  | `KO _ -> return blank_resource
                ))
          | _ -> Exercise.resource exo expected_file >>= function
              | `OK (r, _) -> return r
              | `KO _ -> return blank_resource
      in
      let answer_str = Resource.content answer_resource in

      let submit_answer = server_function Json.t<string> (fun src ->
        Resource.set_content answer_resource src;
        OnDisk.save_resource (Answers.identifier answers) answer_resource
        >> Lwt_unix.sleep 1.
        >> push_new_answer_function (exo_id, name, File expected_file)
        >> return ()
      )
      in
      let onload =
        {{ fun _ ->
          let open EditorHTML in
          !(%reset) ();
          %editor := Some (%editor_maker %expected_extension);
          let editor = %get_editor () in
          let submit () =
            let src = editor.get_value () in
            Lwt.async (fun () ->
              %submit_answer src >> (
                Manip.replaceChildren %grade_div [%score_box "..." []];
                Lwt_js.sleep 1.
              ) >> (
                editor.console_clear ();
                %display_evaluation_state (Some editor.console_write)
              ))
          in
          %reset := editor.EditorHTML.dispose;
          editor.EditorHTML.set_value %answer_str;
          editor.EditorHTML.set_ok_cb submit
        }}
      in
      return (div ~a:[a_onload onload] [
        p [pcdata ("▶ " ^ I18N.String.(
          answer_expected (in_a_file_named expected_file)))]
      ])
    in

    let witv_as_html expressions =

      let nb = List.length expressions in

      let values = {string array{ Array.make %nb "" }} in

      let onload = {{
        fun _ -> !(%reset) ();
        %reset := (fun () -> ());
        %editor := None;
      }}
      in

      let onchange (i : int) (id : id) =
        {{ fun _ ->
          let elt = ExtDom.get_input_by_id %id in
          %values.(%i) <- Js.to_string (elt##value)
         }}
      in
      let item (i : int) expression =
        let id = fresh_id () in
        p [
          span (template_text_as_html [] expression);
          input ~input_type:`Text ~a:[a_id id; a_onchange (onchange i id)] ()
        ]
      in
      return (div ~a:[a_onload onload] (List.mapi item expressions @ [
        small_button ["OK"] {unit -> unit{ fun () ->
          Lwt.async (fun () ->
            %push_new_values_server_function (%exo_str, %name_str, %values)
              (* FIXME: Why is the gif not displayed here? *)
            >> (
              Manip.replaceChildren %grade_div [p [
                pcdata "..."; EntityHTML.get_progress ()
              ]];
              Lwt_js.sleep 0.5
            ) >> %display_evaluation_state None
          )
        }}
      ]))

    in
    let chooser_as_html choices =
      let onload = {{
        fun _ -> !(%reset) ();
        %reset := fun () -> () }}
      in

      let previous =
        (* FIXME *)
        ""
      in
      let submit = server_function Json.t<string> (fun choice ->
        let idx = ExtPervasives.list_index_of choice choices in
        push_new_choice_function (exo_str, name_str, idx)
        (* FIXME: Why is the gif not displayed here? *)
      )
      in
      let property_selector =
        Raw.select ~a:[a_onload onload] (
          List.map (fun s ->
            let a = if s = previous then [a_selected `Selected] else [] in
            Raw.option ~a (pcdata s)
          ) choices
        )
      in
      ignore {unit{
        let e = To_dom.of_select %property_selector in
        let select = fun _ ->
          Lwt.async (fun () ->
            %submit (Js.to_string e##value)
            >> (
              Manip.replaceChildren %grade_div [p [
                pcdata "..."; EntityHTML.get_progress ()
              ]];
              Lwt_js.sleep 0.5
            ) >> %display_evaluation_state None
          );
          Js._true
        in
        Dom_html.(
          ignore (addEventListener e Event.change (handler select) Js._true)
        );
      }};
      return (div ~a:[a_class ["user_answer"]] [property_selector])
    in

    let context_as_html context =
      let rec aux accu = function
        | TNil -> return (List.rev accu)
        | TCode (s, t) ->
          lwt h = match s with
            | QCM (statements, _) ->
              return (qcm_as_html statements)
            | Grader (expected_file, _, _) ->
              grader_as_html expected_file
            | WITV (expressions, _, _) ->
              witv_as_html expressions
            | Chooser choices ->
              chooser_as_html choices
            | NoGrade ->
              return (span [])
          in
          aux (h :: accu) t
        | TAtom (_, t) -> aux accu t
      in
      aux [] context
    in
    lwt context = context_as_html context in
    let rec stars = function
      | 0 -> ""
      | n -> "*" ^ stars (n - 1)
    in
    let tags_as_html tags =
      p ~a:[a_class ["tags"]] (
        List.map (fun t -> span ~a:[a_class ["tag"]] [pcdata t]) tags
      )
    in

    let results_table rows = I18N.(String.(
      let columns =
        [
          cap identifier, "table_id_column";
          name_label, "table_name_column";
          cap friends, "table_friends_column";
          cap answer, "table_answer_column";
          cap state, "table_state_column";
          cap trace, "table_trace_column"
        ]
      in
      let class_of_col i = snd (List.nth columns i) in
      let thead = thead [
        tr (List.map (fun (f, c) -> th ~a:[a_class [c]] [pcdata f]) columns)
      ]
      in
      let row (l, u, f, a, e, t) =
        let fields =
          List.mapi (fun i s -> td ~a:[a_class [class_of_col i]] [s])
        in
        let a =
          match a with
            | Text s ->
              pcdata s
            | URL u ->
              Raw.a ~a:[a_href (Xml.uri_of_string u)] [pcdata (cap download)]
        in
        let show_trace_button t =
          let t = Str.(split (regexp "\n") t) in
          link_button [I18N.String.see]
            {unit -> unit{ fun () ->
              try
                let editor= %get_editor () in
                editor.EditorHTML.console_clear ();
                editor.EditorHTML.console_write (
                  List.map (fun s -> p [pcdata s]) %t
                )
              with Not_found -> ()
             }}
        in
        tr (fields [pcdata l; pcdata u; pcdata f; a; pcdata e;
                    show_trace_button t])
      in
      let rows = List.map row rows in
      tablex ~a:[a_class ["results_table"]] ~thead [tbody rows]
      ))
    in

    let teacher_space =
      active_div 1. (fun () ->
        exercise_results_of_question_function exo_id name_str >>= function
          | `OK rows -> return [results_table rows]
          | `KO _ -> return [] (* FIXME *)
      )
    in

    return (!codes, div (
      [ h1 [pcdata (title ^ " (" ^ stars difficulty ^ ")")];
        tags_as_html tags
      ]
      @ statements_as_html codes statements
      @ context
      @ [ grade_div ; teacher_space ]
    ))
  in

  let focus_eref =
    Eliom_reference.eref
      ~scope:Eliom_common.default_session_scope
      ~persistent:("focus_" ^ (Str.(global_replace (regexp "/") "__" exo_str)))
      None
  in

  let save_focus = server_function Json.t<string> (fun name ->
    Eliom_reference.set focus_eref (Some name)
  )
  in

  let questions_div = {
    (string,
     (unit, (([ pre ]) elt list * [ div_content ] elt)) server_function
    ) Hashtbl.t
    {
      Hashtbl.create 13
    }}
  in

  let focus_on =
    {string -> bool Lwt.t{ fun (name : string) ->
      match !(%focus) with
        | Some name' when name' = name ->
          return true
        | _ ->
          try_lwt
            %save_focus name >>
            let div = Hashtbl.find %questions_div name in
            lwt codes, div = div () in
            %focus := Some name;
            Manip.replaceChildren %statement_div [div];
            WidgetHTML.display_math ["'central_column'"];
            WidgetHTML.highlight codes;
            return true
          with Not_found -> return false (* Inconsistent name. *)
     }}
  in
  let first_question = ref None in

  let navigation_sidebar description answers editor_maker = Questions.(
    let indent level s =
      let indent =
        Printf.sprintf "padding-left:%fem;" (0.5 *. float_of_int level)
      in
      span ~a:[a_style indent] [pcdata s]
    in
    let rec aux level = function
      | Question q ->
        let name = Statement.flatten_string q.id in
        let title = Statement.flatten_string q.title in
        let d = server_function Json.t<unit> (fun () ->
          statement_as_html
            name title
            q.tags q.difficulty
            q.statement q.context
            answers editor_maker
        )
        in
        if !first_question = None then first_question := Some name;
        let onload = {{ fun _ -> Hashtbl.add %questions_div %name %d }} in
        let onclick =
          {{ fun _ -> Lwt.async (fun () -> %focus_on %name >> return ()) }}
        in
        [p ~a:[
          a_class ["navigation_question"];
          a_onclick onclick;
          a_onload onload
        ] [indent level title]]

      | Section (title, qs) ->
        p ~a:[a_class ["navigation_section"]]
          [indent level (flatten_string title)]
        :: questions_template (level + 1) qs

    and questions_template level qs =
      flatten []
        (fun a s -> a)
        (fun a s -> aux level s @ a) qs
    in

    let download_as_pdf questions =
      let export_as_pdf = server_function Json.t<unit> (fun () ->
        let tex = QuestionsLaTeX.make questions in
        let tmp = Filename.temp_file "hj" ".pdf" in
        (ltry (ExtUnix.pdflatex tex tmp)) >>= function
          | `OK _ -> FileHTTP.send tmp >>= fun url -> return (Some url)
          | `KO _ -> return None
      )
      in
      small_button [I18N.(String.(cap download_pdf))] {unit -> unit{
        fun _ -> Lwt.async (fun () ->
           %export_as_pdf () >>= function
             | None ->
               return ()
             | Some url ->
               return (Dom_html.window##location##assign (Js.string url))
        )
      }}
    in

    lwt master_divs = UserHTTP.teacher_only () >>= function
      | `OK user ->
        let submit_new_src = server_function Json.t<string> (fun s ->
          let r = Resource.make "source.aka" s in
          Exercise.import_resource exo r >>= function
            | `OK _ -> Exercise.update user exo
            | `KO e -> (* FIXME *) return (`KO e)
        )
        in
        let edit : (unit, [ pre ] elt list * [div_content] elt) server_function =
          server_function Json.t<unit> (fun () ->
          lwt exo_src =
            Exercise.resource exo "source.aka" >>= function
              | `OK (r, _) -> return (Resource.content r)
              | `KO _ -> return "" (* FIXME *)
          in
          let onload =
            {#Dom_html.event Js.t -> unit{ fun _ ->
              let open EditorHTML in
              !(%reset) ();
              let editor = %editor_maker ".aka" in
              let submit () =
                let src = editor.get_value () in
                Lwt.async (fun () ->
                  %submit_new_src src
                  >> return (editor.console_clear ())
                )
              in
              %reset := editor.EditorHTML.dispose;
              editor.EditorHTML.set_value %exo_src;
              editor.EditorHTML.set_ok_cb submit
             }}
          in
          return ([], div ~a:[a_onload onload] [])
        )
        in
        let name = "__edit_source__" in
        let onload = {{ fun _ -> Hashtbl.add %questions_div %name %edit }} in
        let onclick =
          {{ fun _ -> Lwt.async (fun () -> %focus_on %name >> return ()) }}
        in
        return [
          p ~a:[
            a_class ["master_corner"];
            a_onclick onclick;
            a_onload onload
          ] [pcdata I18N.String.master_corner]
        ]

      | `KO _ -> return []
    in
    let get_focus_eref = server_function Json.t<unit> (
      fun () -> Eliom_reference.get focus_eref
    )
    in
    let onload =
      {{
        fun _ ->
          Lwt.async (fun () ->
            let load_first () =
              match !(%first_question) with
                | None -> return () (* FIXME: absurd, right? *)
                | Some name -> %focus_on name >> return ()
            in
            %get_focus_eref () >>= function
            | None -> load_first ()
            | Some name -> %focus_on name >>= function
                | true -> return ()
                | false -> load_first ()
          )
      }}
    in
    return (div ~a:[a_onload onload] ([
      div ~a:[a_class ["navigation_exo"]] (
        questions_template 0 description.questions
      );
      download_as_pdf description
    ] @ master_divs))
  )
  in (
    ExerciseHTTP.exercise_questions_function (Exercise.identifier exo)
    >>>= fun description ->
    ExerciseHTTP.get_user_answers exo_str
    >>>= fun answers ->
    let links = navigation_sidebar description answers in
    let statement_viewer editor_maker =
      return statement_div
    in
    return (`OK (links, statement_viewer))
  ) >>= function
    | `OK v ->
      return v
    | `KO e ->
      return ((fun _ -> return (div [])),
              (fun _ -> return (div [pcdata "error"]))) (* FIXME *)

let create_service user ok_page ko_page =
  Eliom_registration.Redirection.register_service
    ~path:["create_exercise"]
    ~get_params:Eliom_parameter.(suffix (list "id" (string "label")))
    (fun id () ->
      Exercise.create user (identifier_of_string_list (List.tl id)) >>= function
        | `OK e -> return (ok_page e)
          (* FIXME: Give a better error message. *)
        | `KO e -> return (ko_page "Error")
    )

let exercise_page id =
  EntityHTML.offer_creation Exercise.make create_service exercise_page id

let () =
  EntityHTML.register_page_maker
    (fun id -> Identifier.(is_prefix Exercise.path id))
    exercise_page

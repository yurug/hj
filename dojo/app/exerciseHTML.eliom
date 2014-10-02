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

{client{
exception Done

type question_div = {
  initialize : (unit -> unit);
  codes      : ([ pre ]) elt list;
  content    : [ div_content ] elt;
}

}}

{server{
type question_div = {
  initialize : (unit -> unit) client_value;
  codes      : ([ pre ]) elt list;
  content    : [ div_content ] elt;
}

}}

let submit_src_answer =
  server_function Json.t<string * string * string * string * string> (
    fun (exo_str, (name : string), answers_str, (expected_file : string), src) ->
      let exo_id = identifier_of_string exo_str in
      let answers_id = identifier_of_string answers_str in
      let resource = Resource.make expected_file src in
      OnDisk.save_resource answers_id resource (fun () ->
        push_new_answer_function (exo_id, name, File expected_file)
        >> return () (* FIXME *)
      ) >> return ()
  )

let focus_erefs = Hashtbl.create 13

let focus_eref exo_str =
  try Hashtbl.find focus_erefs exo_str
  with Not_found ->
    let eref = Eliom_reference.eref
      ~scope:Eliom_common.default_session_scope
      ~persistent:("focus_" ^ (Str.(global_replace (regexp "/") "__" exo_str)))
      None
    in
    Hashtbl.add focus_erefs exo_str eref;
    eref

let get_focus = server_function Json.t<string> (fun exo_str ->
  let eref = focus_eref exo_str in
  Eliom_reference.get eref
)

let save_focus = server_function Json.t<string * string> (fun (exo_str, name) ->
  let eref = focus_eref exo_str in
  Eliom_reference.set eref (Some name)
)

let exercise_page exo =

  let focus = {string option ref{ ref None }} in
  let reset = {(unit -> unit) ref{ref (fun () -> ()) }} in

  let fire_reset = {unit -> unit{
    fun _ ->
      !(%reset) ();
      %reset := fun () -> ()
  }}
  in

  let exo_id = Exercise.identifier exo in
  let exo_str = Identifier.string_of_identifier exo_id in

  let statement_div = div [] in

  let statement_as_html
      name title
      tags difficulty
      statements context answers editor_maker =

    let name_str : string = name
    and answers_id = Answers.identifier answers in
    let answers_str = string_of_identifier answers_id in

    let codes = ref [] in

    let grade_div = div ~a:[a_class ["score_div"]] [] in

    let editor = {EditorHTML.interface option ref{ ref None }} in
    let get_editor = {unit -> EditorHTML.interface{ fun () ->
      match !(%editor) with
        | None -> raise Not_found
        | Some e -> e
    }}
    in

    let score_box = {string -> [p_content] elt list -> div elt{
      fun score criteria ->
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
        lwt on_update = AnswersHTTP.on_each_update %answers_str in
        try_lwt
          on_update (fun _ ->
             %exercise_evaluation_state_server_function (%exo_str, %name_str)
             >>= function
               | EvaluationBeingProcessed ->
                 return (update_grade_div [%score_box "..." []])
               | EvaluationDone (_, _, _, grade, commands) ->
                 write_trace_on_console grade.trace;
                 List.iter process_command commands;
                 update_grade_div (scores_as_html grade.scores);
                 raise_lwt Done
               | EvaluationFailed ->
                 return (update_grade_div [%score_box "!" []])
               | NoEvaluation ->
                 return (update_grade_div [%score_box "?" []])
        )
        with Done -> return ()
      }}
    in

    let qcm_as_html statements =

      let choices = {int list ref{ ref [] }} in

      let onshow = {{
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
      div ~a:[a_onshow onshow] (List.mapi qcm_item statements @ [
        small_button ["OK"] {unit -> unit{
          let ready = ref true in
          fun () ->
          if !ready then
            Lwt.async (fun () ->
              ready := false;
              %push_new_choices_server_function (%exo_str, %name_str, !(%choices))
              >> (
                Manip.replaceChildren %grade_div [%score_box "..." []];
                %display_evaluation_state None
              ) >> (return (ready := true))
            )
        }}
      ])
    in

    let grader_as_html (expected_file : string) =

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
      let blank_resource = Resource.make expected_file "" in
      let get_exercise_initial_answer a =
        Exercise.resource exo a >>= function
          | `OK (r, _) -> return r
          | `KO _ -> return blank_resource
      in
      lwt initial_answer = get_exercise_initial_answer expected_file in

      lwt answer_resource =
        match answer with
          | Some (Questions.File a, author) ->
            (Answers.resource answers a >>= function
              | `OK (r, _) -> return r
              | `KO _ -> return initial_answer)
          | _ -> return initial_answer
      in
      let answer_str = Resource.content answer_resource in

      let initial_answer_str = Resource.content initial_answer in

      let initialize =
        {unit -> unit{ fun () ->
          Firebug.console##log (Js.string "Onshow!");
          let open EditorHTML in
          !(%reset) ();
          %editor := Some (%editor_maker %expected_extension);
          let editor = %get_editor () in
          let ready = ref true in
          let submit () =
            if !ready then
              let src = editor.get_value () in
              Lwt.async (fun () ->
                ready := false;
                %submit_src_answer (%exo_str, %name_str, %answers_str, %expected_file, src) >> (
                  Manip.replaceChildren %grade_div [%score_box "..." []];
                  Lwt_js.sleep 1.
                ) >> (
                  editor.console_clear ();
                  %display_evaluation_state (Some editor.console_write)
                  >> return (ready := true)
                ))
          in
          let reset_answer () =
            editor.set_value %initial_answer_str
          in

          %reset := editor.EditorHTML.dispose;
          editor.EditorHTML.set_value %answer_str;
          editor.EditorHTML.set_ok_cb submit;
          editor.EditorHTML.set_reset_cb reset_answer;
        }}
      in
      return (initialize, div [
        p [pcdata ("▶ " ^ I18N.String.(
          answer_expected (in_a_file_named expected_file)))]
      ])
    in

    let witv_as_html expressions =

      let nb = List.length expressions in

      let values = {string array{ Array.make %nb "" }} in

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
      return (div (List.mapi item expressions @ [
        small_button ["OK"] {unit -> unit{
          let ready = ref true in
          fun () ->
            if !ready then Lwt.async (fun () ->
              ready := false;
              %push_new_values_server_function (%exo_str, %name_str, %values)
              (* FIXME: Why is the gif not displayed here? *)
              >> (
                Manip.replaceChildren %grade_div [p [
                  pcdata "..."; EntityHTML.get_progress ()
                ]];
                Lwt_js.sleep 0.5
              ) >> %display_evaluation_state None
              >> return (ready := true)
            )
        }}
      ]))

    in
    let chooser_as_html choices =
      let previous =
        (* FIXME *)
        ""
      in
      let property_selector =
        Raw.select (
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
            (* FIXME: Move that elsewhere. *)
            let list_index_of k =
              let rec find i = function
                | [] -> raise Not_found
                | x :: xs when x = k -> i
                | _ :: xs -> find (succ i) xs
              in
              find 0
            in
            let idx = list_index_of(Js.to_string e##value) %choices in
            %push_new_choice_server_function (%exo_str, %name_str, idx)
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
          link_button [I18N.String.see]
            {unit -> unit{ fun () ->
              Lwt.async (fun () ->
                try_lwt
                  (* FIXME: Use a standard JS function. *)
                  let split c s =
                    let b = Buffer.create 23 in
                    let l = ref [] in
                    let flush () =
                      l := Buffer.contents b :: !l;
                      Buffer.clear b
                    in
                    String.iter (fun c' ->
                      if c = c' then flush () else Buffer.add_char b c'
                    ) s;
                    flush ();
                    List.rev !l
                  in
                  lwt trace = %ExerciseHTTP.trace_get_server_function %t in
                  let editor = %get_editor () in
                  editor.EditorHTML.console_clear ();
                  editor.EditorHTML.console_write (
                    let trace = split '\n' trace in
                    List.map (fun s -> p [pcdata s]) trace
                  );
                  return ()
                with Not_found -> return ())
              }}
        in
        tr (fields [pcdata l; pcdata u; pcdata f; a; pcdata e;
                    show_trace_button t])
      in
      let rows = List.map row rows in
      tablex ~a:[a_class ["results_table"]] ~thead [tbody rows]
      ))
    in

    (* FIXME: The following pattern occurs multiple time.
       FIXME: Define a combinator! *)
    let name_str : string = name in

    lwt import_div =
      let import_id = "import_id" in
      let ocb =
        {unit -> unit{
          fun _ ->
            Lwt.async (fun () ->
              let id = (ExtDom.get_input_by_id %import_id)##value in
              let id = Js.to_string id in
              if id <> "" then (
                %exercise_import_answer_server_function (%exo_str, id, %name_str)
                >> %display_evaluation_state None
              ) else return ()
            )
        }}
      in
      Exercise.is_collaborative exo >>= function
        | true ->
          return (
            div ~a:[a_class ["import_div"]] [
              Raw.input ~a:[a_class ["import_input"]; a_id import_id] ();
              small_button [I18N.String.import] ocb
            ])
        | false -> return (div [])
    in

    let teacher_space =
      active_div 15. (fun () ->
        exercise_results_of_question_function exo_id name_str >>= function
          | `OK rows -> return [results_table rows]
          | `KO _ -> return [] (* FIXME *)
      )
    in

    lwt initialize, context =
      let return_html html =
        return (fire_reset, html)
      in
      (* FIXME: There should only be exactly one context. *)
      let rec aux initialize accu = function
        | TNil ->
          return (initialize, List.rev accu)
        | TCode (s, t) ->
          lwt initialize, h = match s with
            | QCM (statements, _) ->
              return_html (qcm_as_html statements)
            | Grader (expected_file, _, _) ->
              grader_as_html expected_file
            | WITV (expressions, _, _) ->
              witv_as_html expressions >>= return_html
            | Chooser choices ->
              chooser_as_html choices >>= return_html
            | NoGrade ->
              return_html (span [])
          in
          aux initialize (h :: accu) t
        | TAtom (_, t) -> aux initialize accu t
      in
      aux ({{ fun () -> () }}) [] context
    in


    let content = div (
      [ h1 [pcdata (title ^ " (" ^ stars difficulty ^ ")")];
        tags_as_html tags
      ]
      @ statements_as_html codes statements
      @ context
      @ [ grade_div ;
          import_div;
          teacher_space ]
      )
    in
    return {
      codes = !codes;
      content;
      initialize
    }
  in

  let questions_div = { (string, question_div) Hashtbl.t {
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
            Firebug.console##log (Js.string ("Focus " ^ name));
            %save_focus (%exo_str, name) >> (
            Firebug.console##log (Js.string ("Save focus done"));
            let qdiv = Hashtbl.find %questions_div name in
            %focus := Some name;
            Manip.replaceChildren %statement_div [qdiv.content];
            Firebug.console##log (Js.string ("Replace children done"));
            qdiv.initialize ();
            Firebug.console##log (Js.string ("Initialization done"));
            WidgetHTML.display_math ["'central_column'"];
            WidgetHTML.highlight qdiv.codes;
            Firebug.console##log (Js.string ("Focus " ^ name ^ " done"));
            return true
            )
          with Not_found ->
            Firebug.console##log (Js.string ("Focus " ^ name ^ " failed"));
            return false (* Inconsistent name. *)
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
        lwt statement_div =
          statement_as_html
            name title
            q.tags q.difficulty
            q.statement q.context
            answers editor_maker
        in
        if !first_question = None then first_question := Some name;
        let onload = {{ fun _ ->
          Firebug.console##log (Js.string ("Load " ^ %name));
          Hashtbl.add %questions_div %name %statement_div
        }}
        in
        let onclick =
          {{ fun _ -> Lwt.async (fun () -> %focus_on %name >> return ()) }}
        in
        return [p ~a:[
          a_class ["navigation_question"];
          a_onclick onclick;
          a_onload onload
        ] [indent level title]]

      | Section (title, qs) ->
        lwt o = questions_template (level + 1) qs in
        return (
          p ~a:[a_class ["navigation_section"]]
            [indent level (flatten_string title)]
          :: o
        )

    and questions_template level qs =
      lwt_flatten []
        (fun a s -> return a)
        (fun a s ->
          lwt d = aux level s in
          return (a @ d)
        ) qs
    in

    let download_as_pdf questions =
      let export_as_pdf = server_function ~timeout:1000. Json.t<unit> (fun () ->
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
        let submit_new_src =
          server_function ~timeout:600. Json.t<string> (fun s ->
          let r = Resource.make "source.aka" s in
          Exercise.(import_resource exo r (fun () ->
            update user exo >> return ())
          ) >> return () (* FIXME *)
        )
        in
        lwt initialize, (edit : [div_content] elt) =
          lwt exo_src =
            Exercise.resource exo "source.aka" >>= function
              | `OK (r, _) -> return (Resource.content r)
              | `KO _ -> return "" (* FIXME *)
          in
          let initialize =
            {unit -> unit{ fun () ->
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
          return (initialize, div [])
        in
        let name = "__edit_source__" in
        let onload = {{ fun _ ->
          let qdiv = {
            content = %edit; initialize = %initialize; codes = []
          }
          in
          Hashtbl.add %questions_div %name qdiv
        }}
        in
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

    lwt share_with =
      let share_id = "share_id" in
      let ocb =
        {unit -> unit{
          fun _ ->
            Lwt.async (fun () ->
              let id = (ExtDom.get_input_by_id %share_id)##value in
              let id = Js.to_string id in
              if id <> "" then (
                %exercise_new_contributor_server_function (%exo_str, id)
              ) else return ()
            )
        }}
      in
      Exercise.is_collaborative exo >>= function
        | true ->
          return (
            div ~a:[a_class ["share_div"]] [
              Raw.input ~a:[a_class ["share_input"]; a_id share_id] ();
              small_button [I18N.String.share] ocb
            ])
        | false -> return (div [])
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
            %get_focus %exo_str >>= function
            | None -> load_first ()
            | Some name -> %focus_on name >>= function
                | true -> return ()
                | false -> load_first ()
          )
      }}
    in
    lwt qdiv = questions_template 0 description.questions in
    return (div ~a:[a_onload onload] ([
      div ~a:[a_class ["navigation_exo"]] (
        qdiv
      );
      download_as_pdf description;
      share_with;
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

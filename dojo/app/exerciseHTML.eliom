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
open WidgetHTML
open ExerciseHTTP
open ExtPervasives

let fresh_id =
  let r = ref 0 in
  fun () -> incr r; "i" ^ string_of_int !r

let exercise_page exo =

  let reset = {(unit -> unit) ref{ref (fun () -> ()) }} in

  let exo_id = Exercise.identifier exo in
  let exo_str = Identifier.string_of_identifier exo_id in

  let statement_div = div [pcdata I18N.String.please_navigate] in

  let statement_as_html
      name title
      tags difficulty
      statements context answers editor_maker =
    let name_str : string = name in

    let codes = ref [] in

    let grade_div = div [] in

    let display_evaluation_state =
      {([Html5_types.div_content_fun] elt list -> unit) option -> unit Lwt.t{
        fun console_write ->
        let open ExerciseHTTP in
        let criteria_as_html = function
          | Automatic -> pcdata "Dojo:"
          | UserDefined s -> pcdata (s ^ ":")
        in
        let scores_as_html scores =
          List.(flatten (map (fun (c, (i, o)) ->
            [ criteria_as_html c; pcdata (Printf.sprintf "%d/%d" i o) ]
          ) scores))
        in
        let update_grade_div h =
          Manip.replaceChildren %grade_div [p h]
        in
        let write_trace_on_console trace =
          match console_write with
            | None -> ()
            | Some write ->
              List.iter (function Message s -> write [p [pcdata s]]) trace
        in
        let rec wait () = ExerciseHTTP.(
          %exercise_evaluation_state_server_function (%exo_str, %name_str)
          >>= function
            | EvaluationBeingProcessed ->
              update_grade_div [pcdata "..."];
              Lwt_js.sleep 1. >> wait ()
            | EvaluationDone (_, _, _, grade) ->
              write_trace_on_console grade.trace;
              return (update_grade_div (scores_as_html grade.scores))
            | EvaluationFailed ->
              return (update_grade_div [pcdata "!"])
            | NoEvaluation ->
              return (update_grade_div [pcdata "?"])
        )
        in
        wait ()
      }}
    in
    let string_template_as_html classes s =
      span ~a:[a_class classes] [pcdata (flatten_string s)]
    in
    let string_template_as_html_code classes s =
      code [pcdata (flatten_string s)]
    in
    let string_template_as_html_latex classes s =
      span [pcdata ("\\(" ^ flatten_string s ^ "\\)")]
    in
    let string_as_html classes s =
      span ~a:(if classes = [] then [] else [a_class classes]) [pcdata s]
    in
    let rec template_text_as_html classes t =
      List.rev (
        flatten []
          (fun a s -> string_as_html classes s :: a)
          (fun a s -> text_as_html classes s @ a) t
      )
    and text_as_html classes = function
      | String s -> [string_template_as_html classes s]
      | Code s -> [string_template_as_html_code classes s]
      | LaTeX s -> [string_template_as_html_latex classes s]
      | Bold t -> template_text_as_html ("bold" :: classes) t
      | Italic t -> template_text_as_html ("italic" :: classes) t
      | RawHTML s ->
        (* FIXME: This pattern of mutual recursion between an element
           FIXME: and its onload event is very common. Make it a
           FIXME: combinator! .*)
        let s = flatten_string s in
        let self = ref None in
        let set_inner_html =
          {{ fun _ ->
            match !(%self) with
              | None -> ()
              | Some x -> (To_dom.of_span x)##innerHTML <- Js.string %s
           }}
        in
        let s = span ~a:[a_onload set_inner_html] [] in
        self := Some s;
        [s]
      | RawLaTeX _ -> []
      | Hlink (url, caption) ->
        let url = flatten_string url
        and caption = string_template_as_html_code [] caption in
        [Raw.a ~a:[a_href (Xml.uri_of_string url)] [caption]]

    in
    let statement_as_html = function
      | Paragraph t ->
        p (template_text_as_html [] t)
      | Verbatim t ->
        pre [pcdata (flatten_string t)]
      | CodeBlock (l, t) ->
        let elt =
          pre [code ~a:[a_class [flatten_string l]] [
            pcdata (flatten_string t)
          ]]
        in
        codes := elt :: !codes;
        elt
    in
    let statements_as_html t =
      List.rev (
        flatten []
          (fun a s -> p [string_as_html [] s] :: a)
          (fun a s -> statement_as_html s :: a)
          t
      )
    in
    let qcm_as_html statements =

      let choices = {int list ref{ ref [] }} in

      let onload = {{
        fun _ -> !(%reset) ();
        %reset := fun () -> () }}
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
              (* FIXME: Why is the gif not displayed here? *)
            >> (
              Manip.replaceChildren %grade_div [p [
                pcdata "..."; EntityHTML.get_progress ()
              ]];
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
          lwt a = Answers.answer_of_question answers name in
          return (Some a)
        with Not_found ->
          return None
      in
      lwt answer_resource =
        let blank_resource = Resource.make expected_file "" in
        match answer with
          | Some (Questions.File a) ->
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
          let editor = %editor_maker %expected_extension in
          let submit () =
            let src = editor.get_value () in
            Lwt.async (fun () ->
              %submit_answer src >> (
                Manip.replaceChildren %grade_div [p [
                  pcdata "..."; EntityHTML.get_progress ()
                ]];
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
        %reset := fun () -> () }}
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
      | n -> "★" ^ stars (n - 1)
    in
    let tags_as_html tags =
      p ~a:[a_class ["tags"]] (
        List.map (fun t -> span ~a:[a_class ["tag"]] [pcdata t]) tags
      )
    in
    return (!codes, div (
      [ h1 [pcdata (title ^ " (" ^ stars difficulty ^ ")")];
        tags_as_html tags
      ]
      @ statements_as_html statements
      @ context
      @ [ grade_div ]
    ))
  in

  let focus = {string option ref{ ref None }} in

  let focus_on =
    fun (name : string) new_statement_div -> {{ fun _ ->
      Lwt.async (fun () ->
        match !(%focus) with
          | Some name' when %name = name' ->
            return ()
          | _ ->
            %focus := Some %name;
            lwt codes, div = %new_statement_div () in
            Manip.replaceChildren %statement_div [div];
            WidgetHTML.display_math ["'central_column'"];
            WidgetHTML.highlight codes;
            return ()
      )
    }}
  in

  let navigation_sidebar description answers editor_maker = Questions.(
    let rec aux = function
      | Question q ->
        let name = Questions.flatten_string q.id in
        let title = Questions.flatten_string q.title in
        let d = server_function Json.t<unit> (fun () ->
          statement_as_html
            name title
            q.tags q.difficulty
            q.statement q.context
            answers editor_maker
        )
        in
        [li
            ~a:[a_onclick (focus_on name d)]
            [pcdata title]
        ]

      | Section (title, qs) ->
        [li [pcdata (flatten_string title); ul (questions_template qs)]]

    and questions_template qs =
      List.rev (
        flatten [] (fun a s -> li [pcdata s] :: a) (fun a s -> aux s @ a) qs
      )
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
        let edit = server_function Json.t<unit> (fun () ->
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
        return [
          p ~a:[a_onclick (focus_on "__edit_source" edit)] [
            pcdata I18N.String.master_corner
          ]
        ]
      | `KO _ -> return []
    in

    return (div ([
      ul (questions_template description.questions);
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

(* -*- tuareg -*- *)

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
    let string_as_html classes s =
      span ~a:[a_class classes] [pcdata s]
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
      | Bold t -> template_text_as_html ("bold" :: classes) t
      | Italic t -> template_text_as_html ("italic" :: classes) t
    in
    let statement_as_html = function
      | Paragraph t -> p (template_text_as_html [] t)
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

      let onload = {{ fun _ -> !(%reset) (); %reset := fun () -> () }} in

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
              | `KO _ -> return blank_resource)
          | _ -> return blank_resource
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
          let editor = %editor_maker () in
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

    let context_as_html context =
      let rec aux accu = function
        | TNil -> return (List.rev accu)
        | TCode (s, t) ->
          lwt h = match s with
            | QCM (statements, _) ->
              return (qcm_as_html statements)
            | Grader (expected_file, _, _) ->
              grader_as_html expected_file
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
    return (div (
      [ h1 [pcdata (title ^ " (" ^ stars difficulty ^ ")")];
        tags_as_html tags
      ]
      @ statements_as_html statements
      @ context
      @ [ grade_div ]
    ))
  in

  let focus = {string option ref{ ref None }} in

  let focus_on_question =
    fun (name : string) new_statement_div -> {{ fun _ ->
      Lwt.async (fun () ->
        match !(%focus) with
          | Some name' when %name = name' ->
            return ()
          | _ ->
            %focus := Some %name;
            lwt div = %new_statement_div () in
            return (Manip.replaceChildren %statement_div [div])
      )
    }}
  in

  let navigation_sidebar questions answers editor_maker = Questions.(
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
            ~a:[a_onclick (focus_on_question name d)]
            [pcdata title]
        ]

      | Section (title, qs) ->
        [li [pcdata (flatten_string title); ul (questions_template qs)]]

    and questions_template qs =
      List.rev (
        flatten [] (fun a s -> li [pcdata s] :: a) (fun a s -> aux s @ a) qs
      )
    in
    ul (aux questions)
  )
  in (
    ExerciseHTTP.exercise_questions_function (Exercise.identifier exo)
    >>>= fun questions ->
    ExerciseHTTP.get_user_answers exo_str
    >>>= fun answers ->
    let links = navigation_sidebar questions answers in
    let statement_viewer editor_maker =
      statement_div
    in
    return (`OK (links, statement_viewer))
  ) >>= function
    | `OK v -> return v
    | `KO e ->
      return ((fun _ -> div []),
              (fun _ -> div [pcdata "error"])) (* FIXME *)

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

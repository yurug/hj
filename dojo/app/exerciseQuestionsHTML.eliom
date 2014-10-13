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
open Statement
open WidgetHTML
open ExerciseHTTP
open ExtPervasives
open StatementHTML
open ExerciseEvaluationStateHTML

let fresh_id =
  let r = ref 0 in
  fun () -> incr r; "i" ^ string_of_int !r

let chooser_as_html display_evaluation_state_now reset editor exo_str name_str choices =
  let onload = {{
    fun _ -> !(%reset) ();
    %reset := fun () -> () }}
  in

  let previous =
        (* FIXME *)
    ""
  in
  let submit = server_function ~timeout:600. Json.t<string> (fun choice ->
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
        >> %display_evaluation_state_now None
      );
      Js._true
    in
    Dom_html.(
      ignore (addEventListener e Event.change (handler select) Js._true)
    );
  }};
  return (div ~a:[a_class ["user_answer"]] [property_selector])

let qcm_as_html display_evaluation_state_now reset editor exo_str name_str statements =

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
    small_button ["OK"] {unit -> unit{
      let ready = ref true in
      fun () ->
        if !ready then
          Lwt.async (fun () ->
            ready := false;
            %push_new_choices_server_function (%exo_str, %name_str, !(%choices))
            >> (
            %display_evaluation_state_now None
            ) >> (return (ready := true))
          )
    }}
  ])

let witv_as_html display_evaluation_state_now reset editor exo_str name_str expressions =

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
    small_button ["OK"] {unit -> unit{
      let ready = ref true in
      fun () ->
        if !ready then Lwt.async (fun () ->
          ready := false;
          %push_new_values_server_function (%exo_str, %name_str, %values)
          (* FIXME: Why is the gif not displayed here? *)
          >> %display_evaluation_state_now None
          >> return (ready := true)
        )
    }}
  ]))

let grader_as_html exo answers editor_maker display_evaluation_state_now reset editor exo_str name_str expected_file =

  let exo_id = Exercise.identifier exo in
  let expected_extension = Str.(
    if string_match (regexp ".*\\(\\..*\\)") expected_file 0 then
      matched_group 1 expected_file
    else
      ""
  )
  in
  lwt answer =
    try_lwt
      lwt a, author = Answers.answer_of_question answers name_str in
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

  let submit_answer =
    server_function ~timeout:3600. Json.t<string> (fun src ->
      Resource.set_content answer_resource src;
      OnDisk.save_resource (Answers.identifier answers) answer_resource (fun () ->
        push_new_answer_function (exo_id, name_str, File expected_file)
        >> return ()
      )
    )
  in
  let onload =
    {{ fun _ ->
          let open EditorHTML in
              !(%reset) ();
              let editor = %editor_maker %expected_extension in
              %editor := Some editor;
              let ready = ref true in
              let submit () =
                if !ready then
                  let src = editor.get_value () in
                  Lwt.async (fun () ->
                    ready := false;
                    %submit_answer src >> (
                      editor.console_clear ();
                      %display_evaluation_state_now (Some editor.console_write)
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
      return (div ~a:[a_onload onload] [
        p [pcdata ("▶ " ^ I18N.String.(
          answer_expected (in_a_file_named expected_file)))]
      ])

let results_table get_editor rows = I18N.(String.(
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
              let editor= %get_editor () in
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

let question_as_html
    exo
    question
    answers
    (reset : (unit -> unit) ref client_value)
    (editor_maker : (string -> EditorHTML.interface) client_value) =

  (** Shortcuts. *)
  let tags = question.tags
  and difficulty = question.difficulty
  and statements = question.statement
  and context = question.context
  and title = Statement.flatten_string question.title
  and name = Statement.flatten_string question.id in
  let name_str : string = name in
  let exo_id = Exercise.identifier exo in
  let exo_str = Identifier.string_of_identifier exo_id in
  let answers_str = string_of_identifier (Answers.identifier answers) in

  let codes = ref [] in

  let editor = {EditorHTML.interface option ref{ ref None }} in
  let get_editor = {unit -> EditorHTML.interface{ fun () ->
    match !(%editor) with
      | None -> raise Not_found
      | Some e -> e
  }}
  in

  let grade_div = div ~a:[a_class ["score_div"]] [] in
  let display_evaluation_state_now =
   {([Html5_types.div_content_fun] elt list -> unit) option -> unit Lwt.t{
      %display_evaluation_state %exo_str %answers_str %name_str %grade_div
   }}
   in

  let context_as_html context =
    let rec aux accu = function
      | TNil -> return (List.rev accu)
      | TCode (s, t) ->
        lwt h = match s with
          | QCM (statements, _) ->
            return (qcm_as_html display_evaluation_state_now reset editor exo_str name_str statements)
          | Grader (expected_file, _, _) ->
            grader_as_html exo answers editor_maker display_evaluation_state_now reset editor exo_str name_str expected_file
          | WITV (expressions, _, _) ->
            witv_as_html display_evaluation_state_now reset editor exo_str name_str expressions
          | Chooser choices ->
            chooser_as_html display_evaluation_state_now reset editor exo_str name_str choices
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
                >> %display_evaluation_state_now None
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
    active_div ~classes:["teacher_space"] 15. (fun () ->
      exercise_results_of_question_function exo_id name_str >>= function
        | `OK rows -> return [results_table get_editor rows]
        | `KO _ -> return [] (* FIXME *)
    )
  in

  return (!codes, div (
    [ h1 [pcdata (title ^ " (" ^ stars difficulty ^ ")")];
      tags_as_html tags
    ]
    @ statements_as_html codes statements
    @ context
    @ [ grade_div ;
        import_div;
        teacher_space ]
  ))

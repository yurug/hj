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

let theeditor = {EditorHTML.interface option ref{ ref None }}

let get_editor = {unit -> EditorHTML.interface{ fun () ->
  match !(%theeditor) with
    | None -> raise Not_found
    | Some e -> e
}}

let editor_maker = {(string -> EditorHTML.interface) ref{
  ref (fun _ -> assert false)
}}

let reset = {(unit -> unit) ref{
  ref (fun () -> ())
}}

let fresh_id =
  let r = ref 0 in
  fun () -> incr r; "i" ^ string_of_int !r

type frame_editor =
  | NoEditor
  | Editor of string (* answers_str *) * string (* question_name *)

let chooser_as_html
    display_evaluation_state_now exo_str name_str choices
    answer
=
  let onload = {{
    fun _ -> !(%reset) ();
    %reset := fun () -> ();
    Lwt.async (fun () -> %display_evaluation_state_now None)
  }}
  in

  let previous_choices =
    match answer with
      | Some (Questions.Choices uchoices) ->
        List.(map (fun idx -> List.nth choices idx) uchoices)
      | _ ->
        [] (* FIXME: Should never happen, right? *)
  in
  let property_selector =
    Raw.select ~a:[a_onload onload] (
      List.map (fun s ->
        let a =
          if List.mem s previous_choices then
            [a_selected `Selected]
          else
            []
        in
        Raw.option ~a (pcdata s)
      ) choices
    )
  in
  ignore {unit{
    let e = To_dom.of_select %property_selector in
    let select = fun _ ->
      Lwt.async (fun () ->
        let idx =
          JsExtPervasives.list_index_of (Js.to_string e##value) %choices
        in
        %push_new_choice_server_function (%exo_str, %name_str, idx)
        >> %display_evaluation_state_now None
      );
      Js._true
    in
    Dom_html.(
      ignore (addEventListener e Event.change (handler select) Js._true)
    );
  }};
  return (div ~a:[a_class ["user_answer"]] [property_selector])

let qcm_as_html
    display_evaluation_state_now
    exo_str name_str statements
    answer
    =

  let previous_choices =
    match answer with
      | Some (Questions.Choices uchoices) ->
        uchoices
      | _ ->
        []
  in

  let choices = {int list ref{ ref %previous_choices }} in

  let onload = {{
    fun _ -> !(%reset) ();
    %reset := (fun () -> ());
    %theeditor := None;
    Lwt.async (fun () -> %display_evaluation_state_now None)
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
    let is_checked =
      if List.mem (i + 1) previous_choices then [a_checked `Checked] else []
    in
    p [
      input ~input_type:`Checkbox ~a:(a_onclick (oc i) :: is_checked) ();
      span (template_text_as_html [] statement)
    ]
  in
  return (div ~a:[a_onload onload] (List.mapi qcm_item statements @ [
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
  ]))

let witv_as_html
    display_evaluation_state_now
    exo_str name_str expressions answer =

  let nb = List.length expressions in

  let previous_values =
    match answer with
      | Some (Questions.GivenValues vs) ->
        Array.of_list vs
      | _ ->
        Array.make nb ""
  in

  let values = {string array{ Array.copy %previous_values }} in

  let onload = {{
    fun _ -> !(%reset) ();
    %reset := (fun () -> ());
    %theeditor := None;
    Lwt.async (fun () -> %display_evaluation_state_now None)
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
      input ~input_type:`Text ~a:[a_id id; a_onchange (onchange i id); a_value (previous_values.(i))] ()
    ]
  in
  return (div ~a:[a_onload onload] (List.mapi item expressions @ [
    small_button ["OK"] {unit -> unit{
      let ready = ref true in
      fun () ->
        if !ready then Lwt.async (fun () ->
          ready := false;
          %push_new_values_server_function (%exo_str, %name_str, %values)
          >> %display_evaluation_state_now None
          >> return (ready := true)
        )
    }}
  ]))

let submit_answer : (string*string*string*string*string, unit) server_function =
  server_function Json.t<string * string * string * string * string>
  (fun (src, expected_file, answers_id, exo_id, name_str) ->
    try_lwt
      let answers_id = Identifier.identifier_of_string answers_id in
      let exo_id = Identifier.identifier_of_string exo_id in
      let answer_resource = Resource.make (expected_file : string) src in
      OnDisk.save_resource answers_id answer_resource (fun () ->
	push_new_answer_function (exo_id, name_str, File expected_file) >> return ()
      ) >> return ()
    with _ -> return ()
  )

let grader_as_html
    exo answers display_evaluation_state_now
    exo_str (name_str : string) (expected_file : string) =

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
  lwt initial_answer =
    Exercise.resource exo expected_file >>= function
      | `OK (r, _) -> return r
      | `KO _ -> return blank_resource
  in
  lwt answer_resource =
    match answer with
      | Some (Questions.File a, author) ->
        (Answers.resource answers a >>= function
          | `OK (r, _) ->
            Ocsigen_messages.errlog (
              Printf.sprintf "There is an answer in resource %s\n"
                (Resource.name r));
            return r
          | `KO _ ->
            Ocsigen_messages.errlog (
              Printf.sprintf "There is no answer, use resource %s\n"
                (Resource.name initial_answer));
            return initial_answer)
      | _ -> return initial_answer
  in
  let answer_str = Resource.content answer_resource in

  let initial_answer_str = Resource.content initial_answer in

  let answers_id = Answers.identifier answers in
  let answers_id_str = Identifier.string_of_identifier answers_id in
  let exo_id_str = Identifier.string_of_identifier exo_id in

  let show_evaluation_state = {(unit -> unit Lwt.t) ref{ ref (fun () -> return ()) }} in
  let onload =
    {{ fun _ ->
      !(%reset) ();
      let open EditorHTML in
      let editor = !(%editor_maker) %expected_extension in
      %show_evaluation_state := (fun () ->
	 Firebug.console##log (Js.string "Clear?");
         editor.console_clear ();
	 Firebug.console##log (Js.string "Display evaluation state now");
         %display_evaluation_state_now (Some editor.console_write) 
      );
      %theeditor := Some editor;
      Lwt.async !(%show_evaluation_state);
      let ready = ref true in
      let submit () =
          if !ready then
            let src = editor.get_value () in
            Lwt.async (fun () ->
	      try_lwt 
		ready := false;
                %submit_answer (src, %expected_file, %answers_id_str, %exo_id_str, %name_str)
              >> !(%show_evaluation_state) ()
		>> return (ready := true)
	with e ->
	  Firebug.console##log (Js.string ("Exn: " ^ Printexc.to_string e));
	  return ()

	    )
      in
      let reset_answer () =
        editor.set_value %initial_answer_str
      in
      %reset := editor.EditorHTML.dispose;
      editor.EditorHTML.set_value %answer_str;
      editor.EditorHTML.set_ok_cb submit;
      editor.EditorHTML.set_reset_cb reset_answer
     }}
  in
  let upload_form =
    let show = {{ fun _ ->
      Lwt.async (fun () -> !(%show_evaluation_state) ())
    }}
    in
    let import fname =
      let dest = OnDisk.resource_real_path answers_id expected_file in
      let commit () =
        OnDisk.commit_resource answers_id answer_resource (fun () ->
          push_new_answer_function (exo_id, name_str, File expected_file)
          >> return ()
        )
      in
      return (dest, commit)
    in
    post_form ~service:(FileHTTP.upload import) (fun f ->
      [
        file_input ~name:f ();
        string_input ~a:[a_onclick show] ~input_type:`Submit ~value:"OK" ()
      ]
    ) ()
  in
  lwt download_button =
    match answer with
      | Some _ ->
        let src = OnDisk.resource_real_path answers_id expected_file in
        lwt url = FileHTTP.send src in
        return (
          Raw.a ~a:[a_href (Xml.uri_of_string url)] [
            I18N.(String.(pcdata (cap download)))
          ]
        )
      | None ->
        return (span [])
  in
  return (div ~a:[a_onload onload] [
    upload_form;
    download_button;
    p [pcdata ("▶ " ^ I18N.String.(
      answer_expected (in_a_file_named expected_file)))]
  ])

(* let results_table get_editor rows = I18N.(String.(
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
              lwt trace  = %ExerciseHTTP.trace_get_server_function %t in
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
*)

let question_as_html exo question answers : ([ pre ] elt list * [ div ] elt) Lwt.t =

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

  let grade_div = div ~a:[a_class ["score_div"]] [] in
  let display_evaluation_state_now =
    {([Html5_types.div_content_fun] elt list -> unit) option -> unit Lwt.t{
      fun r ->
	Firebug.console##log (Js.string "Display evaluation state now called");
        ExerciseEvaluationStateHTML.display_evaluation_state %exo_str %answers_str %name_str  %grade_div r 
   }}
  in
  lwt answer =
    try_lwt
      lwt a, _ = Answers.answer_of_question answers name in
      return (Some a)
    with Not_found ->
      return None
  in

  let context_as_html context =
    let rec aux accu = function
      | TNil -> return (List.rev accu)
      | TCode (s, t) ->
        lwt h = match s with
          | QCM (statements, _) ->
            qcm_as_html display_evaluation_state_now
              exo_str name_str statements answer
          | Grader (expected_file, _, _) ->
            grader_as_html
              exo answers display_evaluation_state_now
              exo_str name_str expected_file
          | WITV (expressions, _, _) ->
            witv_as_html display_evaluation_state_now
              exo_str name_str expressions answer
          | Chooser choices ->
            chooser_as_html display_evaluation_state_now
              exo_str name_str choices answer
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
                %exercise_import_answer_server_function (
                   %exo_str, id, %name_str
                ) >> %display_evaluation_state_now None
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

(*  let teacher_space =
    active_div ~classes:["teacher_space"] 15. (fun () ->
      exercise_results_of_question_function exo_id name_str >>= function
        | `OK rows -> return (Some [results_table get_editor rows])
        | `KO _ -> return (Some []) (* FIXME *)
    )
  in *)

  return (!codes, div (
    [ h1 [pcdata (title ^ " (" ^ stars difficulty ^ ")")];
      tags_as_html tags
    ]
    @ statements_as_html codes statements
    @ context
    @ [ grade_div ;
        import_div
       (* teacher_space *) ] 
  ))

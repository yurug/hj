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
open ExerciseHTTP
open WidgetHTML
open StatementHTML
open ExercisePDFExportHTML
open ExerciseFocusHTML

let navigation_sidebar
    exo (focus_on : (string -> bool Lwt.t) client_value)
    description answers editor_maker
= Questions.(
  let exo_id = Exercise.identifier exo in
  let exo_str = Identifier.string_of_identifier exo_id in

  let first_question = ref None in

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
      if !first_question = None then first_question := Some name;
      let onclick =
        {{ fun _ -> Lwt.async (fun () -> %focus_on %name >> return ()) }}
      in
      [p ~a:[
        a_class ["navigation_question"];
        a_onclick onclick
      ] [indent level title]]

    | Section (title, qs) ->
      p ~a:[a_class ["navigation_section"]]
        [indent level (flatten_string title)]
      :: questions_template (level + 1) qs

  and questions_template level qs =
    flatten []
      (fun a s -> a)
      (fun a s -> a @ aux level s) qs
  in

    (* lwt master_divs = UserHTTP.teacher_only () >>= function *)
    (*   | `OK user -> *)
    (*     let submit_new_src = *)
    (*       server_function ~timeout:600. Json.t<string> (fun s -> *)
    (*       let r = Resource.make "source.aka" s in *)
    (*       Exercise.(import_resource exo r (fun () -> *)
    (*         update user exo >> return ()) *)
    (*       ) >> return () (\* FIXME *\) *)
    (*     ) *)
    (*     in *)
    (*     let edit : (unit, [ pre ] elt list * [div_content] elt) server_function = *)
    (*       server_function ~timeout:600. Json.t<unit> (fun () -> *)
    (*       lwt exo_src = *)
    (*         Exercise.resource exo "source.aka" >>= function *)
    (*           | `OK (r, _) -> return (Resource.content r) *)
    (*           | `KO _ -> return "" (\* FIXME *\) *)
    (*       in *)
    (*       let onload = *)
    (*         {#Dom_html.event Js.t -> unit{ fun _ -> *)
    (*           let open EditorHTML in *)
    (*           !(%reset) (); *)
    (*           let editor = %editor_maker ".aka" in *)
    (*           let submit () = *)
    (*             let src = editor.get_value () in *)
    (*             Lwt.async (fun () -> *)
    (*               %submit_new_src src *)
    (*               >> return (editor.console_clear ()) *)
    (*             ) *)
    (*           in *)
    (*           %reset := editor.EditorHTML.dispose; *)
    (*           editor.EditorHTML.set_value %exo_src; *)
    (*           editor.EditorHTML.set_ok_cb submit *)
    (*          }} *)
    (*       in *)
    (*       return ([], div ~a:[a_onload onload] []) *)
    (*     ) *)
    (*     in *)
    (*     let name = "__edit_source__" in *)
    (*     let onload = {{ fun _ -> Hashtbl.add %questions_div %name %edit }} in *)
    (*     let onclick = *)
    (*       {{ fun _ -> Lwt.async (fun () -> %focus_on %name >> return ()) }} *)
    (*     in *)
    (*     return [ *)
    (*       p ~a:[ *)
    (*         a_class ["master_corner"]; *)
    (*         a_onclick onclick; *)
    (*         a_onload onload *)
    (*       ] [pcdata I18N.String.master_corner] *)
    (*     ] *)

    (*   | `KO _ -> return [] *)
    (* in *)

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
  return (div ~a:[a_onload onload] ([
    div ~a:[a_class ["navigation_exo"]] (
      questions_template 0 description.questions
    );
    download_as_pdf description;
    share_with;
  ]))
)

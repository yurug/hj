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

open ExerciseFocusHTML
open ExerciseNavigationHTML
open ExerciseQuestionsHTML

let exercise_page exo =

  let focus = {string option ref{ref None }} in
  let reset = {(unit -> unit) ref{ref (fun () -> ()) }} in

  let exo_id = Exercise.identifier exo in
  let exo_str = Identifier.string_of_identifier exo_id in

  let statement_div = div [] in

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
            %save_focus (%exo_str, name) >>
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

  let initialize_questions_div questions answers editor_maker =
    let rec aux level = function
      | Question q ->
        let name = Statement.flatten_string q.id in
        let title = Statement.flatten_string q.title in
        let d = server_function ~timeout:1000. Json.t<unit> (fun () ->
          question_as_html
            exo
            focus reset
            name title
            q.tags q.difficulty
            q.statement q.context
            answers editor_maker
        )
        in
        ignore {unit{ Hashtbl.add %questions_div %name %d }};

      | Section (title, qs) ->
        questions_template (level + 1) qs

    and questions_template level qs =
      flatten ()
        (fun _ s -> ())
        (fun _ s -> aux level s) qs
    in
    questions_template 0 questions

  in ExerciseHTTP.(
    exercise_questions_function (Exercise.identifier exo) >>>= fun qdesc ->
    get_user_answers exo_str >>>= fun answers ->
    let links = navigation_sidebar exo focus_on qdesc answers in
    let statement_viewer editor_maker =
      initialize_questions_div qdesc.questions answers editor_maker;
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

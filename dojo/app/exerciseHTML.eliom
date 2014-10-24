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

exception Found of question

let question_from_name name questions =
  let rec aux = function
    | Question question ->
      let name' = Statement.flatten_string question.id in
      if name = name' then raise (Found question)

    | Section (_, qs) ->
      questions_template qs

  and questions_template qs =
    flatten () (fun _ s -> ()) (fun _ s -> aux s) qs
  in
  try
    questions_template questions;
    None
  with Found q -> Some q

let get_question_div = server_function Json.t<string * string> (
  fun (exo_str, name) ->
    (let exo_id = Identifier.identifier_of_string exo_str in
     Exercise.make exo_id               >>>= fun exo ->
     exercise_questions_function exo_id >>>= fun qdesc ->
     get_user_answers exo_str           >>>= fun answers ->
     match question_from_name name qdesc.questions with
       | Some question ->
         lwt d = question_as_html exo question answers in
         return (`OK d)
       | None ->
         return (`KO `NoSuchQuestion)
    ) >>= function
      | `OK d -> return d
      | `KO e -> assert false (* FIXME *)
)

let exercise_page exo =

  let focus = {string option ref{ref None }} in

  let exo_id = Exercise.identifier exo in
  let exo_str = Identifier.string_of_identifier exo_id in

  let statement_div = div [] in

  let focus_on =
    {string -> bool Lwt.t{ fun (name : string) ->
      match !(%focus) with
        | Some name' when name' = name ->
          return true
        | _ ->
          try_lwt
            %save_focus (%exo_str, name) >>
            lwt codes, div = %get_question_div (%exo_str, name) in
            %focus := Some name;
            Manip.replaceChildren %statement_div [div];
            WidgetHTML.display_math ["'central_column'"];
            WidgetHTML.highlight codes;
            return true
          with Not_found -> return false (* Inconsistent name. *)
     }}
  in

  ExerciseHTTP.(
    exercise_questions_function (Exercise.identifier exo) >>>= fun qdesc ->
    get_user_answers exo_str >>>= fun answers ->
    let links = navigation_sidebar exo focus_on qdesc answers in
    let statement_viewer editor_maker0 =
      ignore {unit{ %ExerciseQuestionsHTML.editor_maker := %editor_maker0 }};
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

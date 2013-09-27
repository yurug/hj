(* -*- tuareg -*- *)

open Lwt
open Eliom_content.Html5
open Eliom_content.Html5.D

open CORE_question

let question_page q =
  return (div [pcdata "question!"])

let question_page =
  HTML_entity.offer_creation CORE_question.make create_service question_page

let () =
  HTML_entity.register_page_maker
    (fun id -> CORE_identifier.(is_prefix (make [label "questions"]) id))
    question_page

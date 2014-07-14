(* -*- tuareg -*- *)

open Lwt
open Eliom_content
open Html5.D

open CORE_message
open CORE_identifier

let as_html onclick =
  (* FIXME: Broken in eliom 4. *)
  assert false

  (* let rec message m = *)
  (*   let onclick_cb = {{ fun _ -> %onclick %m }} in *)
  (*   match m with *)
  (*     | Notification n -> *)
  (*       notification onclick_cb n *)

  (* and notification onclick_cb = function *)
  (*   | EvaluationNeeded id -> *)
  (*     a ~a:[a_onclick onclick_cb] ~service:HTTP_services.page_of [ *)
  (*       pcdata ( *)
  (*         I18N.String.evaluation_needed_for_exercise (string_of_identifier id) *)
  (*       ) *)
  (*     ] (string_list_of_identifier id) *)
  (* in *)
  (* message *)

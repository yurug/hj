(* -*- tuareg -*- *)

(** HTML pages for entity. *)

open Lwt
open Eliom_content.Html5.D
open Html5_types

open CORE_identifier

(** [register_page_maker detect retrieve] publishes a way to
    [detect] if some identifier [id] can have its page using
    [retrieve id]. *)
val register_page_maker:
  (identifier -> bool) ->
  (identifier -> [ body_content ] elt Lwt.t) ->
  unit

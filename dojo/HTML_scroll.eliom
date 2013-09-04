(* -*- tuareg -*- *)

(** The Swiss Army Knife widget. *)

open Lwt
open Eliom_content.Html5.D
open Eliom_content.Html5.Id
open Eliom_lib

type div = [ Html5_types.body_content_fun ] Eliom_content.Html5.D.elt

type hackojo_scroll = {
  elt      : div;
  subs     : div;
  commands : div;
}

let elt_of_hackojo_scroll x = x.elt

let hackojo_scroll
    status short_description ?(start_shown=true) description commands =
  let subs =
    div ~a:[a_class ["scroll_item_subs"]] []
  in
  let description =
    div ~a:[ a_class [ "scroll_description" ]] [ description ]
  in
  let (expand_button, description) =
    HTML_widget.show_or_hide ~start_shown description
  in
  let commands =
    div ~a:[ a_class [ "scroll_commands" ]] (expand_button :: commands)
  in
  let short_description =
    div ~a:[ a_class [ "scroll_short_description" ]] [ short_description ];
  in
  let elt = div ~a:[ a_class [ "scroll_entry" ]] [
        div ~a:[ a_class [ "scroll_status"; "scroll_side" ]] [ status ];
        div ~a:[ a_class [ "scroll_item" ]] [
          div ~a:[ a_class [ "scroll_item_main" ]] [
            short_description;
            commands;
            description
          ];
          subs
        ]
      ]
  in
  let scroll = {
    elt = (elt :> div);
    subs = subs;
    commands = commands;
  }
  in
  scroll

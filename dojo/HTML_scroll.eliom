(* -*- tuareg -*- *)

(** The Swiss Army Knife widget. *)

{shared{
open Lwt
open Eliom_content.Html5.D
open Eliom_content.Html5.Id
open Eliom_lib
open CORE_client_reaction
}}

type div = [ Html5_types.div ] Eliom_content.Html5.D.elt

{shared{

type request =
  | Push of HTML_remote_fragment.idx
deriving (Json)

}}

type hackojo_scroll = {
  elt      : div;
  subs     : div;
  commands : div;
  send     : request -> unit
}

let elt_of_hackojo_scroll x =
  x.elt

let create_subscroll () =
  let id = new_elt_id () in
  let subs = create_named_elt ~id (div ~a:[a_class ["scroll_item_subs"]] []) in
  lwt (reaction, sender) =
    CORE_client_reaction.listening Json.t<request>
    (fun c -> {{ install_automatic_client_reaction %c (function
      | Push idx ->
        lwt elt = HTML_remote_fragment.remote_get idx in
        let open Eliom_content.Html5 in
        Lwt.return (Manip.Named.appendChild %id elt)
     ) }})
  in
  return (subs, sender)


let hackojo_scroll
    (status : div)
    (short_description : div)
    ?(start_shown=true)
    ?(description:div=div[])
    commands =
  lwt (subs, send) = create_subscroll () in
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
    send = send
  }
  in
  return scroll

let push (elt : div) =
  Push (HTML_remote_fragment.local_push (elt :> HTML_remote_fragment.elt))

let from_server request s =
  s.send request

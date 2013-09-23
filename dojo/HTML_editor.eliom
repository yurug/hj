(* -*- tuareg -*- *)

(** A widget for user edition with immediate feedback. *)
{shared{
open Lwt
open Eliom_content.Html5
open Eliom_content.Html5.D

type 'a local_process = (
  (string -> unit) -> string -> 'a option Lwt.t
) client_value

type 'i remote_process = ('i, unit) server_function

}}

{client{

  module Ace = struct
    open Js
    open Js.Unsafe

    class type session = object
      method setMode : js_string t -> unit meth
      method on : js_string t -> ('a, 'b) meth_callback -> unit meth
    end

    class type editor = object
      method getSession : session t meth
      method getValue : js_string t meth
      method setValue : js_string t -> unit meth
    end

    let make id : editor Js.t =
      let ace = Js.Unsafe.variable "ace_library" in
      let e = (coerce ace)##edit (Js.string id) in
      e

  end

}}

let create
    (init           : string)
    (local_process  : 'a local_process)
    (remote_process : 'a remote_process)
=
  let id = "editor" in
  let message_box = div ~a:[a_class ["editor_message"]] [] in
  let echo = {string -> unit{ fun s ->
    Eliom_content.Html5.Manip.replaceAllChild %message_box [
      pcdata s
    ]
  }} in
  let on_load = a_onload {#Dom_html.event Js.t -> unit{ fun _ ->
      let open Js.Unsafe in
      let open Lwt in
      let editor = Ace.make %id in
      let hi = Js.wrap_callback (fun _ ->
        (%local_process %echo (Js.to_string (editor##getValue ())) >>= function
          | None -> return ()
          | Some v -> %remote_process v);
         Js._false
      )
      in
      let session = editor##getSession () in
      session##on (Js.string "change", hi);
      editor##setValue (Js.string %init)
  }}
  in
  let editor = div ~a:[a_class ["editor_box"]] [
    div ~a:[a_id id; on_load; a_class ["editor"]] [];
    message_box
  ] in
  return editor

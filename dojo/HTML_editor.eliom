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

  open Js
  open Js.Unsafe

  class type boolean = object end

  let new_boolean : (bool t -> boolean t) constr =
    Js.Unsafe.variable ("Boolean")

  let true_object = jsnew new_boolean (_true)
  let false_object = jsnew new_boolean (_false)

  module Ace = struct

    class type document = object
      method setValue : js_string t -> unit meth
    end

    class type session = object
      method setMode : js_string t -> unit meth
      method setUseSoftTabs : boolean t -> unit meth
      method getDocument : document t meth
      method on : js_string t -> ('a, 'b) meth_callback -> unit meth
    end

    class type editor = object
      method getSession : session t meth
      method getValue : js_string t meth
      method setValue : js_string t -> js_string t meth
      method setReadOnly : boolean t -> unit meth
    end

    let make id : editor Js.t =
      let ace = Js.Unsafe.variable "ace_library" in
      let e = (coerce ace)##edit (Js.string id) in
      e

    let get =
      let h = Hashtbl.create 13 in
      fun id ->
        try
          Hashtbl.find h id
        with Not_found ->
          let e = make id in
          Hashtbl.add h id e;
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
      let editor = Ace.get %id in
      let hi = Js.wrap_callback (
        let nb = ref 0 in fun _ ->
          incr nb;
          let nb_now = !nb in
          (Lwt_js.sleep 0.5 >>
            if !nb = nb_now then
              (%local_process %echo (Js.to_string (editor##getValue ())) >>= function
                | None -> return ()
                | Some v -> %remote_process v)
            else return ());
         Js._false
      )
      in
      let session = editor##getSession () in
      session##on (Js.string "change", hi);
      session##setUseSoftTabs (true_object);
      ignore (editor##setValue (Js.string %init))
  }}
  in
  let editor = div ~a:[a_class ["editor_box"]] [
    div ~a:[a_id id; on_load; a_class ["editor"]] [];
    message_box
  ] in
  return (editor, id)

{client{

  let refresh id content =
    let editor = Ace.get id in
    if String.compare (Js.to_string (editor##getValue ())) content <> 0 then (
      let s = editor##getSession () in
      let d = s##getDocument () in
      d##setValue (Js.string content);
    )

}}

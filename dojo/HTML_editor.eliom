(* -*- tuareg -*- *)

(** A widget for user edition with immediate feedback. *)
open Lwt
open Eliom_content.Html5
open Eliom_content.Html5.D

{shared{

type 'a local_process = (
  string -> 'a option Lwt.t
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
    end

    let make id : editor Js.t =
      let ace = Js.Unsafe.variable "ace" in
      let e = (coerce ace)##edit (Js.string id) in
      e

  end

}}

let create
    (local_process  : 'a local_process)
    (remote_process : 'a remote_process)
=
  let id = "editor" in
  let on_load = a_onload {#Dom_html.event Js.t -> unit{ fun _ ->
      let open Js.Unsafe in
      let open Lwt in
      let e = Ace.make %id in
      let hi = Js.wrap_callback (fun _ ->
        (%local_process (Js.to_string (e##getValue ())) >>= function
          | None -> return ()
          | Some v -> %remote_process v);
         Js._false
      )
      in
      let session = e##getSession () in
      session##on (Js.string "change", hi)
  }}
  in
  let editor = div [ div ~a:[a_id id; on_load; a_class ["editor"]] []] in
  return editor

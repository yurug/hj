(* -*- tuareg -*- *)

(** A widget for user edition with immediate feedback. *)
{shared{
open Lwt
open Eliom_content.Html5
open Eliom_content.Html5.D

type user_request =
  | Confirm of string * (unit, unit) server_function

type 'a local_process = (
  (string -> unit) -> string -> 'a option Lwt.t
) client_value

type 'i remote_process = ('i, user_request list) server_function
}}

let confirm msg what =
  Confirm (msg, what)

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

    let h = Hashtbl.create 13

    let get =
      fun id ->
        try
          fst (Hashtbl.find h id)
        with Not_found ->
          let e = make id in
          Hashtbl.add h id (e, None);
          e

    let set_last_update id c =
      Hashtbl.replace h id (get id, c)

    let get_last_update id =
      try
        let up = snd (Hashtbl.find h id) in
        set_last_update id None;
        up
      with Not_found -> None

  end

}}

let fresh_editor_id =
  let c = ref 0 in
  fun () -> incr c; "editor" ^ string_of_int !c

let create
    (init           : string)
    (local_process  : 'a local_process)
    (remote_process : 'a remote_process)
=
  let id = fresh_editor_id () in
  let message_box = div ~a:[a_class ["editor_message"]] [] in
  let echo = {string -> unit{ fun s ->
    Manip.replaceAllChild %message_box [
      pcdata s
    ]
  }} in
  let questions_box = div ~a:[a_class ["editor_questions_box"]] [] in
  let process_request = {user_request -> unit Lwt.t{
    let button id (label, what) =
      let onclick = fun _ ->
        Lwt.async what;
        Manip.removeChild %questions_box (Id.get_element id)
      in
      span ~a:[a_class ["editor_message_button"];
               a_onclick onclick] [pcdata label]
    in
    let question msg buttons =
      let id = Id.new_elt_id () in
      Id.create_named_elt ~id (div ~a:[a_class ["editor_message"]] (
        pcdata msg :: List.map (button id) buttons
      ))
    in
    let push e = return (Manip.appendChild %questions_box e) in
    function
    | Confirm (msg, what) ->
      push (question msg [ I18N.String.no, (fun _ -> return ());
                           I18N.String.yes, what; ])
  }}
  in
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
              let content = Js.to_string (editor##getValue ()) in
              let process () = (%local_process %echo content >>= function
                | None ->
                  return ()
                | Some v ->
                  lwt urqs = %remote_process v in
                  Lwt_list.iter_s %process_request urqs)
              in
              match Ace.get_last_update %id with
                | None -> process ()
                | Some c when c <> content -> process ()
                | _ -> return ()
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
    message_box;
    questions_box
  ] in
  return (editor, id)

{client{

  let refresh id content =
    let editor = Ace.get id in
    if String.compare (Js.to_string (editor##getValue ())) content <> 0 then (
      let s = editor##getSession () in
      let d = s##getDocument () in
      Ace.set_last_update id (Some content);
      d##setValue (Js.string content);
    )

}}

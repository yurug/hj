(* -*- tuareg -*- *)

(** A widget for user edition with immediate feedback. *)

{shared{
open CORE_errors
open Lwt
open Eliom_content.Html5
open Eliom_content.Html5.D

(** The widget can be requested to do the following actions: *)
type user_request =
  (** - it can ask for a confirmation about an action is about to do. *)
  | Confirm of string * (unit, user_request list) server_function

  (** - it can transmit a message. *)
  | Message of string

(** The widget's behavior is split between the client and the server. *)

(** Locally, the widget can process its content and may return some
    value of type ['a] out of this content. *)
type 'a local_process = (
  (string -> unit) -> string -> 'a option Lwt.t
) client_value

(** If some value has been extracted from the content, it is processed
    remotely on the server. This process may produce a list of new
    requests to be process by the widget. *)
type 'i remote_process = ('i, user_request list) server_function
}}

let confirm msg what =
  Confirm (msg, what)

let message msg =
  Message msg

(** A minimal binding to CodeMirror. *)
{client{

  open Js
  open Js.Unsafe
  open CORE_errors

  class type boolean = object end

  let new_boolean : (bool t -> boolean t) constr =
    Js.Unsafe.variable ("Boolean")

  let true_object = jsnew new_boolean (_true)
  let false_object = jsnew new_boolean (_false)

  class type js_object = object end

  module CodeMirror = struct

    class type editor = object
      method on : js_string t -> ('a, 'b) meth_callback -> unit meth
      method getValue : js_string t meth
    end

    let make ta =
      let codemirror = variable "CodeMirror" in
      (coerce codemirror)##fromTextArea (ta)

  end

}}

(** A fresh identifier generator. *)
let fresh_editor_id =
  let c = ref 0 in
  fun () -> incr c; "editor" ^ string_of_int !c

(** [create init local remote] instantiates a widget whose content is
    [init] and whose behavior is the composition of [local] and
    [remote]. *)
let create
    (init           : string)
    (local_process  : 'a local_process)
    (remote_process : 'a remote_process)
    =

  (** The widget is composed of three parts:
      - an editor
      - a message box
      - a question box.
  *)

  (** Message box. *)
  (** ------------ *)

  (** The message displays only one string at a time. *)
  let message_box = div ~a:[a_class ["editor_message"]] [] in
  let echo = {string -> unit{ fun s ->
    Manip.replaceAllChild %message_box [
      pcdata s
    ]
   }} in

  (** Question box. *)
  (** ------------- *)

  (** The question box manages a stack of questions that may be
      ignored by the user. (A question disappears after some
      seconds.)

      A question is defined by a string and a list of buttons
      associated to callbacks. If this list if empty, the question
      is merely an information message that disappears after some
      time.
  *)
  let questions_box = div ~a:[a_class ["editor_questions_box"]] [] in

  let process_request = {user_request -> unit Lwt.t{
    let now () = (jsnew Js.date_now ())##valueOf () in
    let hello, bye, lookup =
      let h = Hashtbl.create 13 in
      let hello s = Hashtbl.add h s (now ()) in
      let bye s = Hashtbl.remove h s in
      let lookup s = try Some (Hashtbl.find h s) with Not_found -> None in
      (hello, bye, lookup)
    in
    let button process_request msg id (label, what) =
      let onclick = fun _ ->
        Lwt.async (fun () ->
          lwt rqs = what () in
          Lwt_list.iter_s process_request rqs
        );
        Manip.removeChild %questions_box (Id.get_element id);
        bye msg
      in
      span ~a:[a_class ["editor_message_button"];
               a_onclick onclick] [pcdata label]
    in
    let question process_request ?(timeout = 10.) msg buttons =
      match lookup msg with
        | Some _ -> None
        | None ->
          hello msg;
          let id = Id.new_elt_id ~global:false () in
          let onload _ =
            Lwt.async (fun () -> Lwt_js.sleep timeout >>= fun _ -> (
              Manip.removeChild %questions_box (Id.get_element id);
              return (bye msg)
            )
            )
          in
          Some (Id.create_named_elt ~id
                  (div
                     ~a:[a_class ["editor_message"];
                         a_onload onload
                        ] (
                       pcdata msg
                       :: List.map (button process_request msg id) buttons
                     )))
    in
    let message aux msg = question aux ~timeout:10. msg [] in
    let push e =
      return (match e with
        | None -> ()
        | Some e -> Manip.appendChild %questions_box e
      )
    in
    let rec aux =
      function
        | Confirm (msg, what) ->
          push (question aux msg [ I18N.String.no, (fun _ -> return []);
                                   I18N.String.yes, what; ])
        | Message msg ->
          push (message aux msg)
    in
    aux
  }}
  in

  (** Editor. *)
  (** ------- *)

  (** The editor is implemented by a third party library called "CodeMirror".
      A previous implementation used a library called "Ace" but it happened
      to interact badly with ocsigen (for strange reasons). *)

  let editor_id = fresh_editor_id () in
  let on_load = a_onload {#Dom_html.event Js.t -> unit{ fun _ ->
      let open Js.Unsafe in
      let open Lwt in

      let hi editor = Js.wrap_callback (
        let nb = ref 0 in fun _ ->
          incr nb;
          let nb_now = !nb in
          ignore (Lwt_js.sleep 3. >>= fun _ -> (
            if !nb = nb_now then
              let content = Js.to_string (editor##getValue ()) in
              let process () = (%local_process %echo content >>= function
                | None ->
                  return ()
                | Some v ->
                  lwt urqs = %remote_process v in
                  Lwt_list.iter_s %process_request urqs)
              in
              process ()
            else
              return ()));
         Js._false
      )
      in
      let e = Js.Opt.get (
        Dom_html.document##getElementById (Js.string %editor_id)
      ) (fun _ -> To_dom.of_div (div [
        pcdata "getElementById failed for textarea editor."
      ]))
      in
      let i = CodeMirror.make e in
      i##on (Js.string "change", hi i)
  }}
  in
  let editor_textarea =
    Eliom_content_core.Html5.D.textarea
      ~a:[a_id editor_id; on_load; a_class ["editor"]] (pcdata init)
  in
  let editor = div ~a:[a_class ["editor_box"]] [
    editor_textarea;
    (*  FIXME: Reenable this:
    message_box*)
    questions_box
  ] in
  return (editor, editor_id, process_request)

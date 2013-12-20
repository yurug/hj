(* -*- tuareg -*- *)

(** A widget for user edition with immediate feedback. *)
{shared{
open CORE_errors
open Lwt
open Eliom_content.Html5
open Eliom_content.Html5.D

type user_request =
  | Confirm of string * (unit, user_request list) server_function
  | Message of string
  | Patch   of CORE_errors.position * CORE_errors.position * string

type 'a local_process = (
  (string -> unit) -> string -> 'a option Lwt.t
) client_value

type 'i remote_process = ('i, user_request list) server_function
}}

let confirm msg what =
  Confirm (msg, what)

let message msg =
  Message msg

let patch start stop what =
  Patch (start, stop, what)

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

  module Ace = struct

    class type range = object end

    let new_range
        : (int -> int -> int -> int -> range t) constr =
      Js.Unsafe.variable ("ace_range")

    class type document = object
      method setValue : js_string t -> unit meth
      method replace : range t -> js_string t -> js_object meth
    end

    class type session = object
      method setMode : js_string t -> unit meth
      method setUseSoftTabs : boolean t -> unit meth
      method getDocument : document t meth
      method on : js_string t -> ('a, 'b) meth_callback -> unit meth
      method replace : range t -> js_string t -> js_object meth
      method remove : range t -> js_object meth
    end

    class type editor = object
      method getSession : session t meth
      method getValue : js_string t meth
      method setValue : js_string t -> js_string t meth
      method setReadOnly : boolean t -> unit meth
      method setTheme : js_string t -> unit meth
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

    let range_from_lexing_position start stop =
      jsnew new_range (start.line - 1, start.character - 1,
                       stop.line - 1, stop.character)

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
  let editor_id = fresh_editor_id () in
  let message_box = div ~a:[a_class ["editor_message"]] [] in
  let echo = {string -> unit{ fun s ->
    Manip.replaceAllChild %message_box [
      pcdata s
    ]
  }} in

  let push_job = {(unit -> unit Lwt.t) -> unit{
    let jobs, push = Lwt_stream.create () in
    Lwt.async (fun () -> Lwt_stream.iter_s (fun job -> job ()) jobs);
    fun job -> push (Some job)
  }} in

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
                 pcdata msg :: List.map (button process_request msg id) buttons
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

        | Patch (start, stop, what) ->
          (** Ace prevents modifications of the document inside
              on_change handlers, which is quite reasonable.
              We push the patches as background jobs. *)
          return (%push_job (fun () ->
            Firebug.console##log ("Patching...");
            (** FIXME: Remove the following hack: *)
            Lwt_js.sleep 0.5 >>= fun _ ->
            let e = Ace.get %editor_id in
            let s = e##getSession () in
            let r = Ace.range_from_lexing_position start stop in
            ignore (s##replace (r, Js.string what));
            return ()
          ))
    in
    aux
  }}
  in
  let on_load = a_onload {#Dom_html.event Js.t -> unit{ fun _ ->
      let open Js.Unsafe in
      let open Lwt in
      let editor = Ace.get %editor_id in
      let hi = Js.wrap_callback (
        let nb = ref 0 in fun _ ->
          incr nb;
          let nb_now = !nb in
          ignore (Lwt_js.sleep 0.5 >>= fun _ -> (
            if !nb = nb_now then
              let content = Js.to_string (editor##getValue ()) in
              let process () = (%local_process %echo content >>= function
                | None ->
                  return ()
                | Some v ->
                  lwt urqs = %remote_process v in
                  Lwt_list.iter_s %process_request urqs)
              in
              match Ace.get_last_update %editor_id with
                | None -> process ()
                | Some c when c <> content -> process ()
                | _ -> return ()
            else return ()));
         Js._false
      )
      in
      let session = editor##getSession () in
      session##on (Js.string "change", hi);
      session##setUseSoftTabs (true_object);
      ignore (editor##setValue (Js.string %init)
    )
  }}
  in
  let editor = div ~a:[a_class ["editor_box"]] [
    div ~a:[a_id editor_id; on_load; a_class ["editor"]] [];
    message_box;
    questions_box
  ] in
  return (editor, editor_id, process_request)

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

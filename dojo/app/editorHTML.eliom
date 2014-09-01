(* -*- tuareg -*- *)

(** A widget for user edition with immediate feedback. *)

{shared{
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
      method setValue : js_string t -> unit meth
      method setBooleanOption : js_string t -> boolean -> unit meth
      method toTextArea : unit -> unit meth
    end

    let make ta =
      (* FIXME: Define a cleaner binding. *)
      eval_string (
        Printf.sprintf
          "CodeMirror.fromTextArea(document.getElementById(\"%s\"), {
           lineNumbers: true
           });" ta)

  end

}}

(** A fresh identifier generator. *)
{client{
let fresh_editor_id =
  let c = ref 0 in
  fun () -> incr c; "editor" ^ string_of_int !c
}}

{shared{
type interface = {
  get_value : unit -> string;
  set_value : string -> unit;
  dispose   : unit -> unit;
  set_ok_cb : (unit -> unit) -> unit;
  console_clear : unit -> unit;
  console_write : [ Html5_types.div_content_fun ] elt list -> unit;
}
}}

{client{
exception NoEditor
}}

let make = {string -> interface{ fun (container_id : string) ->
  let editor_id = fresh_editor_id () in
  let elt = Dom_html.(createTextarea document) in
  let container_elt = ExtDom.get_element_by_id container_id in
  let onclick_cb = ref (fun () -> ()) in
  let console_div = div [] in
  let console = To_dom.of_div console_div in
  let button_div = WidgetHTML.small_button ["OK"] (fun _ -> !onclick_cb ()) in
  let button = To_dom.of_div (div ~a:[a_class ["editorbutton"]] [button_div]) in
  elt##id <- Js.string editor_id;
  ignore (container_elt##appendChild (elt));
  ignore (container_elt##appendChild (button));
  ignore (container_elt##appendChild (console));
  let editor = CodeMirror.make editor_id in
  let get_value () = Js.to_string (editor##getValue ()) in
  let set_value s  = editor##setValue (Js.string s) in
  let dispose () =
    editor##toTextArea ();
    ignore (container_elt##removeChild (elt));
    ignore (container_elt##removeChild (button));
    ignore (container_elt##removeChild (console))
  in

  let console_write es =
    Manip.appendChildren console_div es
  in
  let console_clear () =
    Manip.replaceChildren console_div []
  in
  let set_ok_cb f = onclick_cb := f in
  { get_value; set_value; dispose; set_ok_cb;
    console_write; console_clear }
}}
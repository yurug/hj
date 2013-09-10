(* -*- tuareg -*- *)

(** Reactive HTML fragments. *)

(** This module implements primitives to define HTML fragments that
    are asynchronously updated on the client in reaction to
    server-side computations. *)

{shared{
open Lwt
open Eliom_content
open Html5
open Html5.D
open Html5_types
open COMMON_pervasives
open CORE_client_reaction

module EltProduct : MapProduct_sig with type 'a t = 'a elt

(** The communication channel. *)
type ('a, 'b) c

}}

(** The following two functions are meant to be used together in
    a pattern of the form:

    [[
    async_elt init json_type computation (fun c ->
       {unit {react %c reaction}}
    )
    ]]

    This hybrid expression evaluates into an HTML5 element that
    is updated on the client by the client-side function [reaction]
    each time the function [computation] calls an update function
    that is passed as its first argument.

    For instance,

    [[
    let x = ref 0 in
    let server_tick
      let x = ref 0 in
      fun update ->
        let rec aux () = (incr x; update !x) >> sleep 1. >> aux () in
        aux ()
    in
    async_elt (div []) Json.t<int> server_tick (fun c ->
      {unit {react %c (fun x -> return (div [p [pcdata (string_of_int x)]]))}})
    ]]

    generates an endless process on the server that ticks every second
    and only broadcasts the number of ticks while, on the client-side,
    a piece of HTML5 element is computed to perform the layout of this
    integer.

    This pattern is the basic block to properly implement a clear
    separation between layout and content computations.
*)

(** [async_elt elt computation reaction] produces an HTML5
    element that is updated in [reaction] to a [computation].

    An update of a type is conveyed from the [computation] to the
    [reaction]. In response to this notification, the [reaction] must
    produce a new HTML5 element of the same type as the initial
    element [elt]. *)
val async_elt :
  'a Eliom_content_core.Html5.elt
  -> (('b -> unit) -> unit Lwt.t)
  -> (('a only, 'b) c -> reaction Eliom_pervasives.client_value)
  -> 'a Eliom_content_core.Html5.elt Lwt.t

val async_elts :
  'a EltProduct.prod
  -> (('b -> unit) -> unit Lwt.t)
  -> (('a, 'b) c -> reaction Eliom_pervasives.client_value)
  -> 'a EltProduct.prod Lwt.t

  {client{

  (** [react c behavior] describes a client reaction in order to update
      a set of HTML5 elements in response to a value sent through the
      channel [c]. *)
  val react : ('a, 'b) c -> ('b -> 'a EltProduct.prod Lwt.t) -> reaction

  (** [on_background elt behavior] executes [behavior stop]
      continuously unless [stop] is called by [behavior]. *)
  val on_background :
    'a Eliom_content_core.Html5.elt
    -> ((unit -> unit Lwt.t) -> 'a Eliom_content_core.Html5.elt Lwt.t)
    -> unit

}}

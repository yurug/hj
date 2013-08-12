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

(** The communication channel. *)
type 'a c
}}

(** The following two functions are meant to be used together in
    a pattern of the form:

    [[
    async_div json_type computation (fun c ->
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
    async_div Json.t<int> server_tick (fun c ->
      {unit {react %c (fun x -> return (div [p [pcdata (string_of_int x)]]))}})
    ]]

    generates an endless process on the server that ticks every second
    and only broadcasts the number of ticks while, on the client-side,
    a piece of HTML5 element is computed to perform the layout of this
    integer.

    This pattern is the basic block to properly implement a clear
    separation between layout and content computations.
*)
{client{
       val react : 'a c -> ('a -> [> div] elt Lwt.t) -> unit
}}

val async_div :
  'a Deriving_Json.t
  -> (('a -> unit) -> unit Lwt.t)
  -> ('a c -> unit Eliom_pervasives.client_value)
  -> [> div] elt Lwt.t

(* -*- tuareg -*- *)

(** Low-level actions on the client side. *)

(** This module provides low-level combinators to have server-side
    processes controlled by the client. *)

open Lwt
open Eliom_content
open Html5
open Html5.D

{shared{
(** A [client_guardian] must be called on the client side to
    execute the server-side process it guards. *)
type client_guardian = (unit -> unit) client_value
}}

(** [guard w] takes a (server-side) parameterized process [w] and
    returns a pair [(p, c)] where [p] is the process [w] that waits
    for the client function [c] to be applied. *)
val guard : ('a -> 'b Lwt.t) -> ('a -> 'b Lwt.t) * client_guardian

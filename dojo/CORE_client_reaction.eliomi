(* -*- tuareg -*- *)

(** Low-level reactions on the client side. *)

(** This module implements a low-level asynchronous communication
    system between the server and the client. It is used to
    implement the reactive pattern described in {!HTML_reactive}. *)

{shared{
type 'a c
type reaction
}}

{client{
val react : 'a c -> ('a -> unit Lwt.t) -> reaction
}}

val channel : unit -> 'a c * ('a -> unit)

val listening :
  ('a c -> reaction Eliom_pervasives.client_value) ->
  (unit Eliom_pervasives.client_value * ('a -> unit)) Lwt.t

val on :
  (('a -> unit) -> unit Lwt.t) ->
  ('a c -> reaction Eliom_pervasives.client_value) ->
  unit Eliom_pervasives.client_value Lwt.t

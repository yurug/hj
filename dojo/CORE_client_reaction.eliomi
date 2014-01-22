(* -*- tuareg -*- *)

(** Low-level reactions on the client side. *)

(** This module implements a low-level asynchronous communication
    system between the server and the client. It is used to
    implement the reactive pattern described in {!HTML_reactive}. *)

{shared{
type 'a c = 'a Lwt_stream.t
type reaction
}}

{client{
val react
  : ?condition:unit Lwt_condition.t
  -> 'a c list -> ('a -> unit Lwt.t) -> reaction
val react_on_background
  : ?condition:unit Lwt_condition.t
  -> 'a c list -> ('a -> unit Lwt.t) -> reaction
}}

val channel : unit -> 'a c * ('a -> unit)

val clone : 'a c -> 'a c

val listening :
  ('a c -> reaction Eliom_pervasives.client_value) ->
  (unit Eliom_pervasives.client_value * ('a -> unit)) Lwt.t

val on :
  (('a -> unit) -> unit Lwt.t) ->
  ('a c -> reaction Eliom_pervasives.client_value) ->
  unit Eliom_pervasives.client_value Lwt.t

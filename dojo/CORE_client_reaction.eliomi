(* -*- tuareg -*- *)

(** Low-level reactions on the client side. *)

(** This module implements a low-level asynchronous communication
    system between the server and the client. It is used to
    implement the reactive pattern described in {!HTML_reactive}. *)

{shared{
type 'a c
}}

{client{
val install_automatic_client_reaction: 'a c -> ('a -> unit Lwt.t) -> unit
}}

val on :
  'a Deriving_Json.t ->
  (('a -> unit) -> unit Lwt.t) ->
  ('a c -> unit Eliom_pervasives.client_value) ->
  unit Eliom_pervasives.client_value Lwt.t

(* -*- tuareg -*- *)


(** Low-level reactions on the client side. *)

open Lwt

{shared{
type 'a c = 'a Eliom_comet.Channel.t
type reaction = unit
}}

(** Given a process [p] being executed on the server side, we want to
    get a process on the client side that reacts to every signal
    emitted by [p] to denote its update. This update is described
    using a serializable type of data. *)

{client{
  let react channel reaction =
    let channel' = Lwt_stream.clone channel in
    Eliom_client.onload (fun () ->
      Lwt.async (fun () -> Lwt_stream.iter_s reaction channel')
    )

  let react_on_background = react

}}

let channel () : 'a c * ('a -> unit) =
  let stream, push = Lwt_stream.create () in
  let channel = Eliom_comet.Channel.create ~scope:`Site stream in
  (channel, (fun x -> push (Some x)))

let listening
    (reaction    : 'a c -> unit Eliom_pervasives.client_value)
    =
  let channel, sender = channel () in
  return (reaction channel, sender)

let on
    (computation : ('a -> unit) -> unit Lwt.t)
    (reaction    : 'a c -> unit Eliom_pervasives.client_value)
    =
  lwt (reaction, sender) = listening reaction in
  Lwt.async (fun () -> computation sender);
  return (reaction)

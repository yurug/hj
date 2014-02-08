(* -*- tuareg -*- *)


(** Low-level reactions on the client side. *)

{shared{
open Lwt
type 'a c = 'a Lwt_stream.t
type reaction = unit
}}

(** Some reactions must be triggered by a user event because
    the client cannot reallistically react to every event on
    the server. Once a background reaction has been triggered,
    it is active during the following time. *)

(** Given a process [p] being executed on the server side, we want to
    get a process on the client side that reacts to every signal
    emitted by [p] to denote its update. This update is described
    using a serializable type of data. *)

{client{
  let react ?condition msg channels reaction =
    let channels' = List.map Lwt_stream.clone channels in
    let all = Lwt_stream.choose channels' in

    let reaction =
      fun x ->
        try_lwt
          Ocsigen_messages.errlog ("Reacting " ^ msg);
          reaction x
        with e ->
          return (
            Firebug.console##log (Js.string ("react:" ^ Printexc.to_string e))
          )
    in
    Lwt.async (fun () ->
      Lwt_stream.iter_s reaction all >>
        return (Firebug.console##log (Js.string "You should never see this."))
    )

  let react_on_background = react

}}

let channel () : 'a c * ('a -> unit) =
  let stream, push = Lwt_stream.create () in
  (stream, (fun x -> push (Some x)))

let clone = Lwt_stream.clone

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

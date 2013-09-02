(* -*- tuareg -*- *)


(** Low-level reactions on the client side. *)

open Lwt

{shared{
type 'a c = 'a Eliom_bus.t
type reaction = unit
}}

(** Given a process [p] being executed on the server side, we want to
    get a process on the client side that reacts to every signal
    emitted by [p] to denote its update. This update is described
    using a serializable type of data. *)

{client{
  let install_automatic_client_reaction bus reaction =
    Eliom_client.onload (fun () ->
      Lwt.async (fun () -> Lwt_stream.iter_s reaction (Eliom_bus.stream bus))
    )
}}

let on
    (json        : 'a Deriving_Json.t)
    (computation : ('a -> unit) -> unit Lwt.t)
    (reaction    : 'a Eliom_bus.t -> unit Eliom_pervasives.client_value)
    =
  let bus = Eliom_bus.create json in
  Lwt.async (fun () -> computation (fun x -> Eliom_bus.write bus x));
  return (reaction bus)

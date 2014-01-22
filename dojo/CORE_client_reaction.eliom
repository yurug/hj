(* -*- tuareg -*- *)


(** Low-level reactions on the client side. *)

{shared{
open Lwt
type 'a c = 'a Lwt_stream.t
type reaction = unit
}}

(** Given a process [p] being executed on the server side, we want to
    get a process on the client side that reacts to every signal
    emitted by [p] to denote its update. This update is described
    using a serializable type of data. *)

{client{
  let react ?condition channels reaction =
    let channels' = List.map Lwt_stream.clone channels in
    let all = Lwt_stream.choose channels' in
    let reaction x =
      try_lwt
        begin match condition with
          | None -> return ()
          | Some c -> Lwt_condition.wait c
        end
        >> reaction x
      with _ -> return ()
    in
    Eliom_client.onload (fun () ->
      Lwt.async (fun () -> Lwt_stream.iter_s reaction all)
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

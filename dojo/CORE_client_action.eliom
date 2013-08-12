(* -*- tuareg -*- *)

{shared{
open Lwt
type client_guardian = (unit -> unit) client_value
}}

(** [guard w] takes a (server-side) parameterized process [w] and
    returns a pair [(p, c)] where [p] is the process [w] that waits
    for the client function [c] to be applied. *)
let guard server_process =
  let b = Eliom_bus.create Json.t<unit> in
  let s = Eliom_bus.stream b in
  let job x = Lwt_stream.next s >> server_process x in
  let client_trigger = {unit -> unit { fun evt ->
    Lwt.async (fun _ -> Eliom_bus.write %b () >> Lwt.return (Eliom_bus.close %b) )
                                     }}
  in
  (job, client_trigger)

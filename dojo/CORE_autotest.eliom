(* -*- tuareg -*- *)

open Lwt

(** Autotesting service (disabled by default). *)

(** Test infrastructure *)

type test_result =
  | Passed
  | Failed

type test = {
  description : string;
  run         : unit -> test_result Lwt.t
}

let make d f = {
  description = d;
  run = f;
}

let description t = t.description

let run t = t.run ()

(** Testing the server *)

let server_is_up =
  make I18N.String.the_server_is_up (fun () -> return Passed)

let all : test list =  [
  server_is_up
]

(* -*- tuareg -*- *)

open Lwt
open Admin
open Identifier
open HTTP
open UserHTTP

let chroot =
  api_service "chroot" "admin" (string "path") (string "status")

    "Change the root of the system."

    (root_only (fun path ->
      chroot path >>= function
        | `OK () -> completed ()
        | `KO e -> return "error" (* FIXME: give an error code. *)
     ))

let set_registration_condition_command =
  api_service "preregister_hook" "admin" (string "cmd") (string "status")

    "Define a command to check if a login is allowed to register.    \n\
     The command is a UNIX command where '%login' is replaced by the \n\
     login to be checked."

    (root_only (fun cmd ->
      User.set_registration_condition_command cmd;
      completed ()
     ))

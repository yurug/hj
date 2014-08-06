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

let set_user_info_command =
  api_service "user_info_command" "admin" (string "cmd") (string "status")

    "Define a command to get information about a login.              \n\
     The command is a UNIX command where '%login' is replaced by the \n\
     login and %what is replaced by one of the following fields:     \n\
     exists, status, firstname, name, email.                         \n\
    "

    (root_only (fun cmd ->
      User.set_user_info_command cmd;
      completed ()
     ))

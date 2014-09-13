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
  api_service "get_user_info" "admin" (string "cmd") (string "status")

    "Define a command to get information about a login.              \n\
     The command is a UNIX command where '%login' is replaced by the \n\
     login and %what is replaced by one of the following fields:     \n\
     exists, status, firstname, name, email.                         \n\
    "

    (root_only (fun cmd ->
      User.set_user_info_command cmd;
      completed ()
     ))


let set_admin_email  =
  api_service "set_admin_email" "admin" (string "email") (string "status")

    "Define the administrator email."

    (root_only (fun s ->
      Config.set_administrator_email s;
      completed ()
     ))

let set_mailer_command =
  api_service "set_mailer" "admin" (string "cmd") (string "status")

    "Define the system mailer command (default is: /usr/sbin/sendmail)."

    (root_only (fun s ->
      Config.set_mailer s;
      completed ()
     ))

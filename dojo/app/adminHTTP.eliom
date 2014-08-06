(* -*- tuareg -*- *)

open Lwt
open Admin
open Identifier
open HTTP
open UserHTTP

let chroot =
  api_service "chroot" "admin" (string "path") (string "status")
    "Change the root of the system. "
    (root_only (fun path ->
      chroot path >>= function
        | `OK () -> return "completed"
        | `KO e -> return "error" (* FIXME: give an error code. *)
     ))

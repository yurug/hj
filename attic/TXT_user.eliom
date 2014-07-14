(* -*- tuareg -*- *)

open Lwt

open CORE_user

open TXT_services
open TXT_utils

let _login =
  command register_login login login_fallback
    (fun _ _ _ ->
      logged_user () >>= function
      | `FailedLogin | `NotLogged -> return "Login failed."
      | `Logged _ -> return "Logged in.")

let _logout =
  command register_logout logout logout_fallback
    (fun _ _ _ -> return "Logged out.")

(** [whoami] simply returns the connected username. *)
let whoami_service =
  register_request "whoami"
    (fun () ->
      logged_user () >>= (function
      | `NotLogged | `FailedLogin -> return_txt "Nobody (to me)"
      | `Logged u -> lwt l = CORE_user.login u in return_txt l
    ))

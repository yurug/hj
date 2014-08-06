(* -*- tuareg -*- *)

open Lwt
open User
open Identifier

(** The connected username is a server-side state which is shared with
    the client using a cookie. If I am correct, this piece of
    information cannot be forged without a knowledge of the
    password. Of course, if the user is stupid enough to have his
    cookies stolen, we cannot do anything for him.  (Seriously, is
    that too much to ask?) *)

type cookie_state =
  [ `NotLogged | `FailedLogin | `Logged of identifier ]

let username =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    ~persistent:"login_status"
    (`NotLogged : cookie_state)

let user () =
  Eliom_reference.get username >>= function
    | `NotLogged | `FailedLogin -> return None
    | `Logged id -> return (Some id)

exception ForbiddenService

let root_only f x =
  user () >>= function
    | Some id when is_admin id ->
      f x
    | _ ->
      ErrorHTTP.set "You must be logged as admin to do that."
      >> raise_lwt ForbiddenService

let login_status () =
  Eliom_reference.get username >>= function
    | `NotLogged -> return "not_logged"
    | `FailedLogin -> return "login_failure"
    | `Logged id -> return ("logged_as(" ^ string_of_identifier id ^ ")")

let login_service = HTTP.(
  api_service "login" "user"
    (string "login" ** string "password")
    (string "status")
    "Log the user in. \
     Create a cookie containing the user status with respect to the system."
    (fun (login, password) ->
      User.authenticate login password >>= (function
        | `OK u -> Eliom_reference.set username (`Logged u)
        | `KO -> Eliom_reference.set username `FailedLogin
      ) >> login_status ())
)

let logout_service = HTTP.(
  api_service "logout" "user"
    unit
    (string "status")
    "Log the user out."
    (fun () ->
      Eliom_reference.set username `NotLogged
      >> login_status ())
)

let whoami = HTTP.(
  api_service "whoami" "user"
    unit
    (string "status")
    "Returns the status of the user with respect to the system."
    (fun () -> login_status ())
)

let register = HTTP.(
  api_service "register" "user"
    (string "login" ** string "password")
    (string "status")
    "Register a user."
    (fun (login, password) ->
      User.register login password >>= (function
        | `OK _ -> return "registered"
        | `KO _ -> return "registration_failure"
      ))

)
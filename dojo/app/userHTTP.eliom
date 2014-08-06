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
    | `NotLogged | `FailedLogin -> None
    | `Logged id -> Some id

let login_status () =
  Eliom_reference.get username >>= function
    | `NotLogged -> return "not logged"
    | `FailedLogin -> return "previous login failed"
    | `Logged id -> return ("logged as " ^ string_of_identifier id)

let login_service = HTTP.(
  api_service "login" "user"
    (string "login" ** string "password")
    (string "status")
    "Log the user in. \
     Create a cookie containing the user status with respect to the system."
    (fun (login, password) ->
      Eliom_reference.set username (match User.authenticate login password with
        | `OK u -> `Logged u
        | `KO -> `FailedLogin
      )
      >> login_status ())
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

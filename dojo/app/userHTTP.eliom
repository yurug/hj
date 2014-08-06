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

let login_status () =
  Eliom_reference.get username >>= function
    | `NotLogged -> return "not logged"
    | `FailedLogin -> return "previous login failed"
    | `Logged id -> return ("logged as " ^ string_of_identifier id)

(** Login is an action on the state of the server. *)
let login_service = HTTP.(
  api_service "login" "user"
    (string "login" ** string "password")
    (string "status")
    "Login"
    (fun (login, password) ->
      Eliom_reference.set username (match User.authenticate login password with
        | `OK u -> `Logged u
        | `KO -> `FailedLogin
      )
      >> login_status ())
)

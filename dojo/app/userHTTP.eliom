(* -*- tuareg -*- *)

open Lwt
open User
open Identifier
open HTTP
open ExtPervasives

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
    | `NotLogged
    | `FailedLogin -> return None
    | `Logged id -> return (Some id)

exception ForbiddenService

let root_only f x =
  user () >>= function
    | Some id when is_admin id ->
      f x
    | _ ->
      ErrorHTTP.set "admin_only" >> raise_lwt ForbiddenService

let logged_user () =
  Eliom_reference.get username >>= function
    | `Logged id -> User.make id
    | `FailedLogin -> return (`KO `FailedLogin)
    | `NotLogged -> return (`KO `NotLogged)

let teacher_only () =
  logged_user () >>>= fun user ->
  User.is_teacher user >>= function
    | true -> return (`OK user)
    | false -> return (`KO `ForbiddenService)

let login_status () =
  Eliom_reference.get username >>= function
    | `NotLogged -> success "not_logged"
    | `FailedLogin -> error "login"
    | `Logged id -> success ("logged_as:" ^ string_of_identifier id)

let login_function (login, password) =
  User.authenticate login password >>= (function
    | `OK u -> Eliom_reference.set username (`Logged u)
    | `KO -> Eliom_reference.set username `FailedLogin
  )

let login_server_function =
  server_function Json.t<string * string> login_function

let login_service = HTTP.(
  api_service "login" "user"
    (string "login" ** string "password")
    (string "status")
    "Log the user in. \
     Create a cookie containing the user status with respect to the system."
    (fun x -> login_function x >> login_status ())
)

let logout_function () =
  Eliom_reference.set username `NotLogged

let logout_server_function =
  server_function Json.t<unit> logout_function

let logout_service = HTTP.(
  api_service "logout" "user"
    unit
    (string "status")
    "Log the user out."
    (fun () -> logout_function () >> login_status ())
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
      root_only (fun () ->
        User.register login password
      ) () >>= (function
        | `OK _ -> success "registered"
        | `KO (`UndefinedEntity id) -> assert false
        | `KO `UnauthorizedLogin -> error "login_cannot_register"
        | `KO (`AlreadyExists _) -> error "already_exists"
        | `KO (`SystemError e) -> error ("system:" ^ e)
        | `KO (`InternalError e) -> error ("internal:" ^ (Printexc.to_string e))
      ))
)

let get_fresh_password_reset_url = ref (fun _ ->
  "<Please report a bug if you see this.>"
)

let update_password_function login =
  User.get_user_info login "exists" >>= function
    | "1" ->
      lwt email = User.get_user_info login "email" in
      lwt firstname = User.get_user_info login "firstname" in
      lwt surname = User.get_user_info login "surname" in
      let url = !get_fresh_password_reset_url login in
      ExtUnix.mail
        ~mailer:(Config.get_mailer ())
        ~domain:Ocsigen_config.server_name
        ~target_email:email
        ~target_name:login
        ~subject:I18N.String.password_reset_email_subject
        ~message:(I18N.String.password_reset_email_body login url firstname surname);
      return (`OK email)
    | _ ->
      return (`KO `UnauthorizedLogin)

let update_password_server_function =
  server_function Json.t<string> (fun login ->
    update_password_function login >>= function
      | `OK email -> return (Some email)
      | `KO `UnauthorizedLogin -> return None
  )

let update_password = HTTP.(
  api_service "update_password" "user"
    (string "login")
    (string "status")
    "Update user password."
    (fun login ->
      update_password_function login >>= (function
        | `OK e -> success ("email_sent_to:" ^ e)
        | `KO (`UndefinedEntity id) -> assert false
        | `KO `UnauthorizedLogin -> error "login_cannot_register"
        | `KO (`AlreadyExists _) -> error "already_exists"
        | `KO (`SystemError e) -> error ("system:" ^ e)
        | `KO (`InternalError e) -> error ("internal:" ^ (Printexc.to_string e))
      ))
)

let set_password = HTTP.(
  api_service "set_password" "user"
    (string "login" ** string "password_digest")
    (string "status")
    "Set user password."
    (fun (login, pd) ->
      root_only (fun () ->
	User.set_password_digest (identifier_of_string login) pd >>= (function
          | `OK _ -> success "password_reset"
          | `KO (`UndefinedEntity id) -> assert false
          | `KO `UnauthorizedLogin -> error "login_cannot_register"
          | `KO (`AlreadyExists _) -> error "already_exists"
          | `KO (`SystemError e) -> error ("system:" ^ e)
          | `KO (`InternalError e) -> error ("internal:" ^ (Printexc.to_string e))
	)) ())
)

let password_reset_server_function =
  server_function Json.t<string * string> (
    fun (login, password) ->
      User.register login password
      >> login_function (login, password)
      >> return ()
  )



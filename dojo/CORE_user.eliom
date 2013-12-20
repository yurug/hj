(* -*- tuareg -*- *)

open Lwt

open CORE_entity
open CORE_inmemory_entity
open CORE_identifier
open CORE_standard_identifiers
open CORE_error_messages
open COMMON_pervasives

type description = {
  login           : string;
  password_digest : string;
  last_connection : string;
  firstname       : string;
  surname         : string;
} deriving (Json)

include CORE_entity.Make (CORE_entity.Passive (struct

  type data = description deriving (Json)

  let string_of_replacement _ = "Update"

end))

let password_digest e = observe e (fun u -> return (content u).password_digest)
let login           e = observe e (fun u -> return (content u).login)
let firstname       e = observe e (fun u -> return (content u).firstname)
let surname         e = observe e (fun u -> return (content u).surname)
let last_connection e = observe e (fun u -> return (content u).last_connection)

(** By convention, users are stored in the "users" folder. *)

let user_id username =
  identifier_of_path (concat users_path (CORE_identifier.make [label username]))

let user =
  let module Limited = Lwt_throttle.Make (struct
    type t = string
    let hash = Hashtbl.hash
    let equal x y = (x = y)
  end) in
  let limit =
    Limited.create
      CORE_config.number_of_login_attempts_per_second
      max_int
      13
  in
  fun username ->
    Limited.wait limit username >>= function
      | false -> return (`KO `MaximalNumberOfLoginAttemptsReached)
      | true ->
        make (user_id username) >>= function
          | `OK u -> return (`OK u)
          | `KO (`AlreadyExists _) -> assert false
          | `KO (`UndefinedEntity _) -> return (`KO `BadLoginPasswordPair)
          | `KO (`SystemError e) -> return (`KO (`SystemError e))

(** Authentification system. *)

(** The connected username is a server-side state which is shared with
    the client using a cookie. If I am correct, this piece of
    information cannot be forged without a knowledge of the
    password. Of course, if the user is stupid enough to have his
    cookies stolen, we cannot do anything for him.
    (Seriously, is that too much to ask?) *)
type cookie_state =
  [ `NotLogged | `FailedLogin | `Logged of identifier ]

let username =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    (`NotLogged : cookie_state)

let logged_user () =
  Eliom_reference.get username >>= function
    | `NotLogged -> return `NotLogged
    | `FailedLogin -> return `FailedLogin
    | `Logged id ->
      make id >>= function
        | `OK u -> return (`Logged u)
        | `KO _ ->
          (** Hmm, if [make e] fails, it means that:
              - the cookie is corrupted ;
              - or, the system is in bad state...
              We log that and do not return a logged user. *)
          COMMON_log.(unexpected_failure [Internal] (fun () -> assert false));
          return `FailedLogin

(** Authentification. *)

let make_password_digest login password =
  Digest.to_hex (Digest.string (login ^ password))

let authenticate u password =
  user u >>>= fun user ->
  lwt login = login user in
  let digest = make_password_digest login password in
  lwt expected_digest = password_digest user in
  if (expected_digest <> digest) then
    return (`KO `BadLoginPasswordPair)
  else (
    lwt c = observe user (fun d -> return (content d)) in
    ltry COMMON_unix.now >>>= fun date ->
    change user (UpdateContent { c with last_connection = date })
    >>= fun _ -> return (`OK user)
  )

(** [login] is in the public API, login information
    is passed using the POST method. *)
let login_service ~fallback =
  Eliom_service.post_service
    ~fallback
    ~post_params:Eliom_parameter.(string "login" ** string "password")
    ()

(** Login is an action on the state of the server. *)
let register_login ~service =
  Eliom_registration.Action.register
    ~service
    (fun () (login, password) ->
      authenticate login password >>= fun r ->
      Eliom_reference.set username (match r with
        | `OK u -> `Logged (identifier u)
        | `KO e -> `FailedLogin
      )
    )

(** Disconnection *)

let logout_service ~fallback =
  Eliom_service.coservice
    ~fallback
    ~get_params:Eliom_parameter.unit
    ()

(** [disconnect] is an action on the state of the server
    which also removes the status of being connected
    in the client's state. *)
let register_logout ~service =
  Eliom_registration.Action.register
    ~service
    (fun () () ->
      Eliom_state.discard ~scope:Eliom_common.default_session_scope ()
      >>= fun _ -> Eliom_reference.set username `NotLogged
    )

(** Subscription. *)

(** [subscribe] is in the public API, registration information
    is passed using the POST method. *)
let subscribe_service ~fallback =
  Eliom_service.post_service
    ~fallback
    ~post_params:Eliom_parameter.(
      string "firstname" ** string "surname"
      ** string "email"
      ** string "login"  ** string "password"
    ) ()

(** Subscribing is an action on the state of the server. *)

let register_subscribe out_by ~service =
  Eliom_registration.Action.register
    ~service
    (fun () (firstname, (surname, (email, (login, password)))) ->
      let id = user_id login in
      let password_digest = make_password_digest login password in
      let last_connection = I18N.String.never_connected_before in
      let dependencies =
        let deps = empty_dependencies in
        (** A user depends on the assigner entity. *)
        let deps = push deps (assigner, ("assigner", [])) in
        deps
      in
      let init =
        ({ login; password_digest; last_connection; firstname; surname },
         dependencies,
         CORE_property.empty,
         [])
      in
      make ~init id >>= function
        | `OK e ->
          Eliom_reference.set username (`Logged (identifier e))
          >>= fun _ -> out_by (`Right ())
        | `KO e -> out_by (`Left (Some (string_of_error e)))
    )

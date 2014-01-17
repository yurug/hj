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
  email           : string;
  teacher         : bool;
  mbox            : CORE_message.mbox
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
let email           e = observe e (fun u -> return (content u).email)

let set on_c e =
  lwt c = observe e (fun u -> return (content u)) in
  change e (UpdateContent (on_c c))

let set_email e email = set (fun c -> { c with email }) e

let change_properties e f =
  lwt s = observe e (fun u -> return (properties u)) in
  change e (UpdateProperties (f s))

let has_property e s =
  observe e (fun u -> return (CORE_property.is s (properties u)))

let properties e =
  observe e (fun u -> return (properties u))

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

let make_password_digest login password =
  Digest.to_hex (Digest.string (login ^ password))

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

(** Subscribing is an action on the state of the server. *)
let subscribe out_by firstname surname email login password teacher =
  let id = user_id login in
  let password_digest = make_password_digest login password in
  let last_connection = I18N.String.never_connected_before in
  let mbox = CORE_message.empty_mbox () in
  let dependencies =
    let deps = empty_dependencies in
    (** A user depends on the assigner entity. *)
    let deps = push deps (assigner, ("assigner", [])) in
    deps
  in
  let init =
    ({ login; password_digest; last_connection;
       firstname; surname; teacher; email; mbox },
     dependencies,
     CORE_property.empty,
     [])
  in
  make ~init id >>= function
    | `OK e ->
      Eliom_reference.set username (`Logged (identifier e))
      >>= fun _ -> out_by (`Right ())
    | `KO e -> out_by (`Left (Some (string_of_error e)))

(** Authentification. *)

open Ldap_funclient
open Ldap_types

let ldap_search login server_config = CORE_config.(
  let filter =
    (* FIXME: Watch for user injection. *)
    server_config.login_field ^"="^ login
  in
  let try_ f =
    try_lwt return (`OK (f ()))
    with e -> return (`KO (`SystemError (Ldap_error.ldap_strerror "" e)))
  in
  (try_ (fun () ->
    init [Printf.sprintf "ldap://%s:%d" server_config.host server_config.port]
   ) >>>= (fun connection ->
     try_ (fun () ->
       let authenticate =
         let who = server_config.username in
         let cred = server_config.password in
         if who <> "" then bind_s ~who ~cred connection
       in
       search_s ~base:server_config.base connection filter
     ) >>>= fun l ->
       let rec aux = function
         | [] ->
           return (`KO (`BadLoginPasswordPair))
         | (`Referral _) :: es ->
           aux es
         | (`Entry e) :: es ->
           if es <> [] then
             Ocsigen_messages.errlog (
               Printf.sprintf
                 "Two entries share the same login in LDAP server %s!"
                 server_config.host
             );
           let lookup field =
             if field = "uid" then
               login
             else try
               let a =
                 List.find (fun a -> a.attr_type = field) e.sr_attributes
               in
               String.concat " " a.attr_vals
             with Not_found -> Ocsigen_messages.errlog (
               Printf.sprintf "Field %s not found in entry %s" field login
             );
             raise Not_found
           in
           try_lwt
             let pretty s = String.(capitalize (lowercase s)) in
             let is_teacher s = Str.(
               string_match (regexp server_config.teacher_status_value) s 0
             )
             in
             let name =
               try
                 lookup server_config.name_field
               with Not_found ->
                 try
                   lookup server_config.fullname_field
                 with Not_found -> ""
             in
             let email =
               try
                 lookup server_config.email_field
               with Not_found ->
                 login ^ "@" ^ server_config.domain
             in
             return (`OK (
               pretty (lookup server_config.firstname_field),
               name,
               email,
               is_teacher (lookup server_config.status_field)
             ))
           with Not_found ->
             Ocsigen_messages.errlog (
               Printf.sprintf
                 "LDAP server %s does not respond as configurated here!"
                 server_config.host
             );
             return (`KO `BadLoginPasswordPair)
       in
       aux l
     >>= fun r -> (
     unbind connection;
     return r
     )
  )))

let rec authenticate u password =
  user u >>= function
    (** The user does exist. *)
    | `OK user ->
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

    (** The user does not exist. *)
    | `KO `BadLoginPasswordPair ->
      let do_subscribe firstname surname email teacher =
        let after = function
          | `Left (Some error) ->
            return (`KO (`SystemError error))
          | `Left None ->
            return (`KO (`SystemError "LDAP"))
          | `Right _ ->
            authenticate u password
        in
        subscribe after firstname surname email u password teacher
      in
      if CORE_config.development_mode () then
        do_subscribe "Donald" "Knuth" "you@shallnotemail.me" true
      else begin
        let error = function
          | [] -> `BadLoginPasswordPair
          | e :: _ -> e
        in
        first_success (ldap_search u) error (CORE_config.ldap_servers ())
        >>= function
          | `OK (firstname, surname, email, teacher) ->
            do_subscribe firstname surname email teacher
          | `KO e ->
            return (`KO e)
      end

    (** Something else happens. *)
    | (`KO (`MaximalNumberOfLoginAttemptsReached | `SystemError _)) as e ->
      return e

let logged_user =
  let first_time = ref 10 in
  fun () ->
  Eliom_reference.get username >>= function
    | `NotLogged ->
      if CORE_config.development_mode () && !first_time > 0 then (
        decr first_time;
        authenticate "donald" "no_password_for_me" >>= function
          | `OK u -> return (`Logged u)
          | `KO _ -> return `NotLogged
      ) else
        return `NotLogged
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

let register_subscribe out_by ~service =
  Eliom_registration.Action.register
    ~service
    (fun () (firstname, (surname, (email, (login, password)))) ->
      (** Public subscription cannot create teachers. *)
      let teacher = false in
      subscribe out_by firstname surname email login password teacher
    )

(** [is_teacher u] returns [true] if [u] is a teacher. *)
let is_teacher u = observe u (fun d -> return (content d).teacher)

(** [send u msg] pushes a new message [msg] in the mailbox of [u]. *)
let send u msg =
  Ocsigen_messages.errlog (CORE_message.string_of_message msg);
  lwt c = observe u (fun c -> return (content c)) in
  change u (UpdateContent { c with mbox = CORE_message.push msg c.mbox })

let unread u =
  observe u (fun c -> return (CORE_message.unread (content c).mbox))

let read u =
  observe u (fun c -> return (CORE_message.read (content c).mbox))

let mark_as_read u msg =
  lwt c = observe u (fun c -> return (content c)) in
  change u (UpdateContent {
    c with mbox = CORE_message.mark_as_read msg c.mbox
  })

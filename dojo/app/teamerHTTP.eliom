(* -*- tuareg -*- *)

{shared{
open Lwt
}}
open ExtPervasives
open User
open Teamer
open Identifier
open HTTP
open UserHTTP

let teamer_create = HTTP.(
  api_service "teamer_create" "teamer"
    (string "name")
    (string "status")
    "Create a fresh teamer dispatcher."
    (fun name ->
      (teacher_only () >>>= fun user ->
       Teamer.create user (identifier_of_string name)
      ) >>= function
        | `OK _ -> completed ()
        | `KO `NotLogged -> error "not_logged"
        | `KO `FailedLogin -> error "login_failed"
        | `KO (`AlreadyExists _) -> error "already_exists"
        | `KO (`SystemError e) -> error ("system:" ^ e)
        | `KO (`InternalError e) -> error ("internal:" ^ (Printexc.to_string e))
        | `KO `StudentsCannotCreateTeamer -> error "teacher_only"
        | `KO `ForbiddenService -> error "teacher_only"
        | `KO (`UndefinedEntity id) ->
          error ("undefined:" ^ (string_of_identifier id)))
)

let (upload_resource, upload_tar, download_resource,
     ls_resource, publish_resource, download_public_resource) =
  EntityHTTP.create_resource_management_api
    (module Teamer)
    "teamer_upload"
    "teamer_upload_tar"
    "teamer_download"
    "teamer_ls"
    "teamer_publish"
    "tpub"
    "teamer"

let teamer_update = HTTP.(
  api_service "teamer_update" "teamer"
    (string "identifier")
    (string "status")
    "Trigger a teamer update from resource 'teamer.json'."
    (fun name ->
      (teacher_only () >>>= fun user ->
       Teamer.make (identifier_of_string name) >>>= fun teamerd ->
       Teamer.update user teamerd
      ) >>= function
        | `OK _ -> completed ()
        | `KO (`InvalidDescription e) -> success e
        | `KO `NotLogged -> error "not_logged"
        | `KO `FailedLogin -> error "login_failed"
        | `KO (`AlreadyExists _) -> error "already_exists"
        | `KO (`SystemError e) -> error ("system:" ^ e)
        | `KO (`InternalError e) -> error ("internal:" ^ (Printexc.to_string e))
        | `KO `StudentsCannotCreateTeamer -> error "teacher_only"
        | `KO `ForbiddenService -> error "teacher_only"
        | `KO (`UndefinedEntity id) ->
          error ("undefined:" ^ (string_of_identifier id)))
)

let teamer_reserve_for_user_function name sid uid slot =
  logged_user () >>>= fun user ->
  Teamer.make name >>>= fun teamer ->
  Teamer.reserve_for_user teamer user sid uid slot

let teamer_reserve_for_user_server_function =
  server_function Json.t<string * string * string * int> (
    fun (name, sid, uid, slot) ->
      let name = (identifier_of_string name)
      and uid = (identifier_of_string uid) in
      (teamer_reserve_for_user_function name (SID sid) slot uid >>= function
        | `OK () -> return true
        | `KO _  -> return false)
  )

let teamer_reserve_for_user = HTTP.(
  api_service "teamer_reserve_for_user" "teamer"
    (string "identifier" ** string "sid" ** string "uid" ** int "slot")
    (string "status")
    "Reserve a slot for a user."
    (fun (name, (sid, (uid, slot))) ->
      let uid = identifier_of_string uid in
      let name = identifier_of_string name in
      (teamer_reserve_for_user_function name (SID sid) slot uid) >>= function
        | `OK _ -> completed ()
        | `KO `MustBeAlreadyInTheTeam -> error "must_already_be_in_the_team"
        | `KO `AlreadyInATeam -> error "already_in_a_team"
        | `KO `TeamIsFull -> error "team_is_full"
        | `KO (`UndefinedSubjectIdentifier _) -> error "undefined_subject_identifier"
        | `KO `NotLogged -> error "not_logged"
        | `KO `FailedLogin -> error "login_failed"
        | `KO (`AlreadyExists _) -> error "already_exists"
        | `KO (`SystemError e) -> error ("system:" ^ e)
        | `KO (`InternalError e) -> error ("internal:" ^ (Printexc.to_string e))
        | `KO `StudentsCannotCreateTeamer -> error "teacher_only"
        | `KO `ForbiddenService -> error "teacher_only"
        | `KO (`UndefinedEntity id) ->
          error ("undefined:" ^ (string_of_identifier id)))
)

let teamer_confirm_for_user_function name sid slot_idx uid =
  logged_user () >>>= fun user ->
  Teamer.make name >>>= fun teamer ->
  (if Identifier.compare (User.identifier user) uid <> 0 then
      return (`KO `OnlyTheUserCanConfirm)
   else
      return (`OK ())
  ) >>>= fun () ->
  confirm teamer sid slot_idx uid
  >> return (`OK ())

let teamer_confirm_for_user_server_function =
  server_function Json.t<string * string * string * int> (
    fun (name, sid, uid, slot) ->
      let name = (identifier_of_string name)
      and uid = (identifier_of_string uid) in
      (teamer_confirm_for_user_function name (SID sid) slot uid >>= function
        | `OK () -> return true
        | `KO _  -> return false)
  )

let teamer_confirm_for_user = HTTP.(
  api_service "teamer_confirm_for_user" "teamer"
    (string "identifier" ** string "sid" ** string "uid" ** int "slot")
    (string "status")
    "Reserve a slot for a user."
    (fun (name, (sid, (uid, slot))) ->
      let uid = identifier_of_string uid in
      let name = identifier_of_string name in
      (teamer_confirm_for_user_function name (SID sid) slot uid) >>= function
        | `OK _ -> completed ()
        | `KO `MustBeAlreadyInTheTeam -> error "must_already_be_in_the_team"
        | `KO `AlreadyInATeam -> error "already_in_a_team"
        | `KO `OnlyTheUserCanConfirm -> error "only_user_can_confirm"
        | `KO `TeamIsFull -> error "team_is_full"
        | `KO (`UndefinedSubjectIdentifier _) -> error "undefined_subject_identifier"
        | `KO `NotLogged -> error "not_logged"
        | `KO `FailedLogin -> error "login_failed"
        | `KO (`AlreadyExists _) -> error "already_exists"
        | `KO (`SystemError e) -> error ("system:" ^ e)
        | `KO (`InternalError e) -> error ("internal:" ^ (Printexc.to_string e))
        | `KO `StudentsCannotCreateTeamer -> error "teacher_only"
        | `KO `ForbiddenService -> error "teacher_only"
        | `KO (`UndefinedEntity id) ->
          error ("undefined:" ^ (string_of_identifier id)))
)

let teamer_withdraw_for_user_function name sid slot_idx uid =
  logged_user () >>>= fun user ->
  Teamer.make name >>>= fun teamer ->
  (if Identifier.compare (User.identifier user) uid <> 0 then
      return (`KO `OnlyTheUserCanWithdraw)
   else
      return (`OK ())
  ) >>>= fun () ->
  withdraw_for_user teamer sid slot_idx uid
  >> return (`OK ())

let teamer_withdraw_for_user_server_function =
  server_function Json.t<string * string * string * int> (
    fun (name, sid, uid, slot) ->
      let name = (identifier_of_string name)
      and uid = (identifier_of_string uid) in
      (teamer_withdraw_for_user_function name (SID sid) slot uid >>= function
        | `OK () -> return true
        | `KO _  -> return false)
  )

let teamer_withdraw_from_user = HTTP.(
  api_service "teamer_withdraw_for_user" "teamer"
    (string "identifier" ** string "sid" ** string "uid" ** int "slot")
    (string "status")
    "Reserve a slot for a user."
    (fun (name, (sid, (uid, slot))) ->
      let uid = identifier_of_string uid in
      let name = identifier_of_string name in
      (teamer_withdraw_for_user_function name (SID sid) slot uid) >>= function
        | `OK _ -> completed ()
        | `KO `MustBeAlreadyInTheTeam -> error "must_already_be_in_the_team"
        | `KO `AlreadyInATeam -> error "already_in_a_team"
        | `KO `OnlyTheUserCanWithdraw -> error "only_user_can_withdraw"
        | `KO `TeamIsFull -> error "team_is_full"
        | `KO (`UndefinedSubjectIdentifier _) -> error "undefined_subject_identifier"
        | `KO `NotLogged -> error "not_logged"
        | `KO `FailedLogin -> error "login_failed"
        | `KO (`AlreadyExists _) -> error "already_exists"
        | `KO (`SystemError e) -> error ("system:" ^ e)
        | `KO (`InternalError e) -> error ("internal:" ^ (Printexc.to_string e))
        | `KO `StudentsCannotCreateTeamer -> error "teacher_only"
        | `KO `ForbiddenService -> error "teacher_only"
        | `KO (`UndefinedEntity id) ->
          error ("undefined:" ^ (string_of_identifier id)))
)

let email_user uid ~subject ~message =
  User.make uid >>= function
    | `OK user ->
      lwt email = User.email user in
      lwt login = User.login user in
      Printf.eprintf "Email %s %s [%s]:\n%s\n%!"
        email login subject message;
      return (ExtUnix.mail
        ~mailer:(Config.get_mailer ())
        ~domain:Ocsigen_config.server_name
        ~target_email:email
        ~target_name:login
        ~subject
        ~message
      )
    | `KO _ ->
      (* FIXME: Warn *)
      return ()

let teamer_url teamer_id =
  Printf.sprintf "http://%s:%d%s"
    (Eliom_config.get_default_hostname ())
    (Eliom_config.get_default_port ())
    (string_of_identifier teamer_id)

let remind_not_enough_users teamer_id expected given limit uid =
  let teamer_url = teamer_url teamer_id in
  email_user uid
    ~subject:I18N.String.remind_not_enough_users_subject
    ~message:(I18N.String.remind_not_enough_users teamer_url limit expected given)

let remind_confirmation_needed teamer_id limit uid =
  let teamer_url = teamer_url teamer_id in
  email_user uid
    ~subject:I18N.String.remind_confirmation_needed_subject
    ~message:(I18N.String.remind_confirmation_needed teamer_url limit)

let send_cancellation_email teamer_id sid slot_idx uid =
  let teamer_url = teamer_url teamer_id in
  email_user uid
    ~subject:I18N.String.cancellation_email_subject
    ~message:(I18N.String.cancellation_email teamer_url sid slot_idx)

let install_mail_hooks =
  Teamer.remind_not_enough_users    := remind_not_enough_users;
  Teamer.remind_confirmation_needed := remind_confirmation_needed;
  Teamer.send_cancellation_email    := send_cancellation_email

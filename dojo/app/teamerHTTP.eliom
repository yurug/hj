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

let teamer_reserve_for_himself name sid slot =
  logged_user () >>>= fun user ->
  Teamer.make name >>>= fun teamer ->
  let uid = User.identifier user in
  Teamer.reserve_for_user teamer user sid slot uid >>= function
    | `KO `AlreadyInATeam | `OK _ ->
      (Teamer.confirm teamer sid slot uid >> return (`OK ()))
    | `KO e ->
      return (`KO e)

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

let teamer_insert_user_function name sid slot_idx uid =
  logged_user () >>>= fun user ->
  Teamer.make name >>>= fun teamer ->
  lwt user_is_teacher = User.is_teacher user in
  (if Identifier.compare (User.identifier user) uid <> 0 && not user_is_teacher then
      return (`KO `OnlyTeacherCanInsertUser)
   else
      return (`OK ())
  ) >>>= fun () ->
  reserve_for_user teamer user sid slot_idx uid
  >> confirm teamer sid slot_idx uid
  >> return (`OK ())

let teamer_insert_user_server_function =
  server_function Json.t<string * string * string * int> (
    fun (name, sid, uid, slot) ->
      let name = (identifier_of_string name)
      and uid = (identifier_of_string uid) in
      (teamer_insert_user_function name (SID sid) slot uid >>= function
        | `OK () -> return true
        | `KO _  -> return false)
  )

let teamer_withdraw_for_user_function name sid slot_idx uid =
  logged_user () >>>= fun user ->
  Teamer.make name >>>= fun teamer ->
  lwt user_is_teacher = User.is_teacher user in
  (if Identifier.compare (User.identifier user) uid <> 0 && not user_is_teacher then
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

let teamer_versions = HTTP.(
  api_service "teamer_versions" "teamer"
    (string "identifier")
    (string "status")
    "Retrieve all the versions of a teamer."
    (fun name ->
      (let name = identifier_of_string name in
      Teamer.make name >>>= fun teamer ->
      (lwt vs = teamer_versions teamer in
      return (`OK vs))) >>= function
        | `OK vs -> success vs
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
          error ("undefined:" ^ (string_of_identifier id))))

let email_user uid ~subject ~message =
  User.make uid >>= function
    | `OK user ->
      lwt email = User.email user in
      lwt login = User.login user in
      lwt firstname = User.firstname user in
      lwt surname = User.surname user in
      return (ExtUnix.mail
        ~mailer:(Config.get_mailer ())
        ~domain:Ocsigen_config.server_name
        ~target_email:email
        ~target_name:login
        ~subject
        ~message:(message firstname surname)
      )
    | `KO _ ->
      (* FIXME: Warn *)
      return ()

let teamer_url teamer_id =
  Printf.sprintf "http://%s:%d%s"
    (Eliom_config.get_default_hostname ())
    (Eliom_config.get_default_port ())
    (string_of_identifier teamer_id)

let teamer_url_to_reserve teamer_id sid cdate =
  Printf.sprintf "http://%s:%d/direct/teamer_reserve%s/%s/%f"
    (Eliom_config.get_default_hostname ())
    (Eliom_config.get_default_port ())
    (string_of_identifier teamer_id)
    sid
    cdate

let remind_not_enough_users teamer_id sid title slot_idx cdate expected given limit uid =
  let cdate = Timestamp.to_float cdate in
  let teamer_url = teamer_url teamer_id in
  let teamer_url_to_reserve = teamer_url_to_reserve teamer_id sid cdate in
  let slot_idx = succ slot_idx in
  email_user uid
    ~subject:I18N.String.remind_not_enough_users_subject
    ~message:(I18N.String.remind_not_enough_users teamer_url teamer_url_to_reserve title slot_idx limit expected given)

let remind_confirmation_needed teamer_id sid slot_idx limit uid =
  let teamer_url = teamer_url teamer_id in
  let slot_idx = succ slot_idx in
  email_user uid
    ~subject:I18N.String.remind_confirmation_needed_subject
    ~message:(I18N.String.remind_confirmation_needed teamer_url sid slot_idx limit)

let send_cancellation_email teamer_id sid slot_idx uid =
  let teamer_url = teamer_url teamer_id in
  let slot_idx = succ slot_idx in
  email_user uid
    ~subject:I18N.String.cancellation_email_subject
    ~message:(I18N.String.cancellation_email teamer_url sid slot_idx)

let send_withdraw_email teamer_id sid slot_idx uid ruid expiration is_complete =
  let expiration = Timestamp.to_string expiration in
  let teamer_url = teamer_url teamer_id in
  let slot_idx = succ slot_idx in
  User.make ruid >>= function
    | `OK ruser ->
      lwt rfirstname = User.firstname ruser in
      lwt rsurname = User.surname ruser in
      email_user uid
        ~subject:I18N.String.withdraw_warning_subject
        ~message:(I18N.String.withdraw_warning
                    teamer_url sid slot_idx rfirstname rsurname
                    expiration is_complete)
    | `KO e ->
      (* FIXME *)
      return ()


let install_mail_hooks =
  Teamer.remind_not_enough_users    := remind_not_enough_users;
  Teamer.remind_confirmation_needed := remind_confirmation_needed;
  Teamer.send_cancellation_email    := send_cancellation_email;
  Teamer.send_withdraw_email        := send_withdraw_email;

open Lwt
open Entity
open InMemory
open Identifier
open ExtPervasives
open ExtProcess

let path = Identifier.from_strings [ "users" ]

let user_id username =
  Identifier.(identifier_of_path (concat path (make [label username])))

let admin_username = "admin"

let admin_id = user_id admin_username

type password_digest = PasswordDigest of string deriving (Json)

let admin_password : password_digest option ref = ref None

let is_admin id = (Identifier.compare id admin_id = 0)

let digest password = PasswordDigest (Digest.string ("hj:" ^ password ^ ":"))

let set_admin_password s =
  admin_password := Some (digest s)

exception NoAdminPasswordSet

let get_admin_password () =
   match !admin_password with
    | None ->
      Error.fatal NoAdminPasswordSet
    | Some p ->
      p

let authenticate_admin password =
  if digest password = get_admin_password () then
    `OK admin_id
  else
    `KO

type internal_state = {
  login           : string;
  logged          : bool;
  password_digest : password_digest;
  teacher         : bool;
  firstname       : string;
  surname         : string;
  email           : string;
} deriving (Json)

include Entity.Make (struct

  type data = internal_state deriving (Json)

  type change =
    | SetPasswordDigest of password_digest
    | Login
    | Logout

  let react state mdeps cs later =
    let make_change content = function
      | SetPasswordDigest password_digest ->
        { content with password_digest }
      | Login ->
        { content with logged = true }
      | Logout ->
        { content with logged = false }
    in
    let content = List.fold_left make_change (content state) cs in
    return (UpdateContent content)

  let current_version = "1.0"

  let converters = []

  let string_of_change = function
    | SetPasswordDigest _ -> "set password"
    | Login  -> "login"
    | Logout -> "logout"

end)

let user_module = "hackojo.user <here@hackojo.org>"

let up () = VFS.(
  if not (exists path) then
    create user_module path
  else
    return (`OK ())
)

let authenticate_standard_user login password =
  let id = user_id login in
  make id >>= function
    | `OK user ->
      observe user (fun state ->
        return (if (digest password = (content state).password_digest) then
          `OK id
        else
          `KO
        )
      )
    | `KO _ ->
      return `KO

let authenticate login password =
  if login = admin_username then
    return (authenticate_admin password)
  else
    authenticate_standard_user login password

let user_info_command = ref (fun login what -> !% "echo 1")

let set_user_info_command cmd =
  user_info_command := fun login what ->
    !% Str.(
      let cmd = global_replace (regexp "%login") login cmd in
      global_replace (regexp "%what") what cmd
    )

let get_user_info login what =
  ltry (fun lraise -> pread ~lraise (!user_info_command login what))
  >>= function
    | `OK s -> return s
    | `KO e -> return "get_user_info_failed" (* FIXME: Critical. *)

let is_teacher login =
  lwt s = get_user_info login "status" in
  return (s = "teacher")

let register login password =
  get_user_info login "exists" >>= function
    | "1\n" ->
      if login = admin_username then
        return (`KO (`AlreadyExists (path_of_identifier admin_id)))
      else
        let id = user_id login in
        let password_digest = digest password in
        lwt teacher = is_teacher login in
        lwt firstname = get_user_info login "firstname" in
        lwt surname = get_user_info login "surname" in
        lwt email = get_user_info login "email" in
        let data = {
          login;
          logged = false;
          password_digest;
          teacher;
          firstname;
          surname;
          email
        }
        in
        make ~init:(data, empty_dependencies, []) id
    | r ->
      return (`KO `UnauthorizedLogin)

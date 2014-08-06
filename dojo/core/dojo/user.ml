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
    logged          : bool;
    password_digest : password_digest
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

let registration_condition_command = ref (fun s -> !% "true")

let set_registration_condition_command cmd =
  registration_condition_command := fun l ->
    !% (Str.(global_replace (regexp "%login") l cmd))

let register login password =
  must_succeed (!registration_condition_command login) >>= function
    | false -> return (`KO `UnauthorizedLogin)
    | true ->
      if login = admin_username then
        return (`KO (`AlreadyExists (path_of_identifier admin_id)))
      else
        let id = user_id login in
        let data = { logged = false; password_digest = digest password } in
        make ~init:(data, empty_dependencies, []) id

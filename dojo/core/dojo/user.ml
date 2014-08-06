open Lwt
open Entity
open InMemory
open Identifier

let path = Identifier.from_strings [ "users" ]

let user_id username =
  Identifier.(identifier_of_path (concat path (make [label username])))

let admin_username = "admin"

let admin_id = user_id admin_username

let admin_password = ref None

let is_admin id = (Identifier.compare id admin_id = 0)

let digest password = Digest.string ("hj:" ^ password ^ ":")

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

let authenticate login password =
  if login = admin_username then
    authenticate_admin password
  else
    `KO

include Entity.Make (struct

  type data = {
    logged : bool
  } deriving (Json)

  type change = Login | Logout

  let react state mdeps changes later =
    return NoUpdate

  let current_version = "1.0"

  let converters = []

  let string_of_change = function
    | Login -> "login"
    | Logout -> "logout"

end)

let user_module = "hackojo.user <here@hackojo.org>"

let up () = VFS.(
  if not (exists path) then
    create user_module path
  else
    return (`OK ())
)

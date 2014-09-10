open Lwt
open Entity
open InMemory
open Identifier
open ExtPervasives
open ExtProcess

let path = Identifier.from_strings [ "users" ]

let user_identifier username =
  Identifier.(identifier_of_path (concat path (make [label username])))

let admin_username = "admin"

let admin_id = user_identifier admin_username

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
  tags            : Tag.set;
} deriving (Json)

type public_change =
  | SetPasswordDigest of password_digest
  | Login
  | Logout
  | Tagged of Identifier.t * Questions.identifier * string list * int

include Entity.Make (struct

  type data = internal_state deriving (Json)

  type change = public_change

  let react state mdeps cs later =
    let make_change content = function
      | SetPasswordDigest password_digest ->
        { content with password_digest }
      | Login ->
        { content with logged = true }
      | Logout ->
        { content with logged = false }
      | Tagged (exo_id, q_id, tags, difficulty) ->
        let who = string_of_identifier exo_id ^ "/" ^ q_id in
        { content with tags = Tag.tag who tags difficulty content.tags }
    in
    let content = List.fold_left make_change (content state) cs in
    return (UpdateContent content)

  let current_version = "1.0"

  let converters = []

  let string_of_change = function
    | SetPasswordDigest _ -> "set password"
    | Login  -> "login"
    | Logout -> "logout"
    | Tagged (exo_id, q_id, tags, difficulty) ->
      Printf.sprintf "tagged %s[%d] by %s/%s"
        (String.concat ", " tags)
        difficulty
        (string_of_identifier exo_id)
        q_id

end)

let is_teacher      e = observe e (fun u -> return (content u).teacher)
let password_digest e = observe e (fun u -> return (content u).password_digest)
let login           e = observe e (fun u -> return (content u).login)
let firstname       e = observe e (fun u -> return (content u).firstname)
let surname         e = observe e (fun u -> return (content u).surname)
let email           e = observe e (fun u -> return (content u).email)

let user_module = "hackojo.user <here@hackojo.org>"

let up () = VFS.(
  if not (exists path) then
    create user_module path
  else
    return (`OK ())
)

let authenticate_standard_user login password =
  let id = user_identifier login in
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
  let remove_trailing_newline s = Str.(global_replace (regexp "\n") "" s) in
  ltry (fun lraise -> pread ~lraise (!user_info_command login what))
  >>= function
    | `OK s -> return (remove_trailing_newline s)
    | `KO e -> return "get_user_info_failed" (* FIXME: Critical. *)

let register login password =
  get_user_info login "exists" >>= function
    | "1" ->
      if login = admin_username then
        return (`KO (`AlreadyExists (path_of_identifier admin_id)))
      else
        let id = user_identifier login in
        let password_digest = digest password in
        lwt status = get_user_info login "status" in
        let teacher = (status = "teacher") in
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
          email;
          tags = Tag.Set.empty
        }
        in
        make ~init:(data, empty_dependencies, []) id
    | r ->
      return (`KO `UnauthorizedLogin)

let tag uid exoid qid tags difficulty =
  make uid >>= function
    | `OK user -> change user (Tagged (exoid, qid, tags, difficulty))
    | _ -> return () (* FIXME *)

let has_tag uid tag =
  make uid >>= function
    | `OK user ->
      observe user (fun state ->
        return (Tag.has_tag tag (content state).tags)
      )
    | `KO _ -> return false (* FIXME *)

let _ =
  AkaInterpreter.user_has_tag :=
    fun s t -> has_tag (identifier_of_string s) t

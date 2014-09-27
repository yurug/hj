open Lwt
open Entity
open InMemory
open Identifier
open ExtPervasives
open ExtProcess

let path = Identifier.from_strings [ "users" ]

let user_identifier username =
  Identifier.(identifier_of_path (concat path (make [label username])))

let all_user_ids () =
  ltry (ExtUnix.ls ~relative:true (VFS.real_path path)) >>= function
    | `OK l ->
      let l = List.filter (fun s -> s <> "." && s <> "..") l in
      return (List.map user_identifier l)
    | `KO _ -> return [] (* FIXME *)

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

type all_notifications =
    (string list * Notifications.notification_identifier) list
deriving (Json)

let all_notifications : all_notifications ref = ref []

let push_new_notification has_tags l =
  let c = (List.sort String.compare has_tags, l) in
  if not (List.mem c !all_notifications) then
    all_notifications := c :: !all_notifications

let all_notifications_filename () =
  Filename.concat (VFS.real_path path) "_all_notifications.json"

let load_all_notifications () = Deriving_Json.(
  ltry (ExtUnix.cat (all_notifications_filename ())) >>= function
    | `OK content ->
      begin try_lwt
        all_notifications := from_string Json.t<all_notifications> content;
        return ()
      with _ -> return () (* FIXME *)
      end
    | `KO _ ->
      return ()
)

let save_all_notifications () =
  try_lwt
    ltry (
      ExtUnix.echo
        (Deriving_Json.to_string Json.t<all_notifications> !all_notifications)
        (all_notifications_filename ())
    )
  with _ ->
    (** happens during logic_shutdown () trigered by the first chroot. *)
    return (`OK ())

let get_current_notifications utags folder = List.(
  let included tags = for_all (fun t -> mem t utags) tags in
  fold_left (fun s (_, n) -> Notifications.push s n) folder (
    filter (fun (tags, _) -> included tags) !all_notifications
  )
)

type internal_state = {
  login           : string;
  logged          : bool;
  password_digest : password_digest;
  teacher         : bool;
  firstname       : string;
  surname         : string;
  email           : string;
  tags            : Tag.set;
  active          : Notifications.folder;
  hidden          : Notifications.folder;
} deriving (Json)

type public_change =
  | SetPasswordDigest of password_digest
  | Login
  | Logout
  | Tagged of Identifier.t * Questions.identifier * string list * int
  | NewNotification of Notifications.notification_identifier
  | HideNotification of Notifications.notification_identifier

include Entity.Make (struct

  type data = internal_state deriving (Json)

  type change = public_change

  let kind = "user"

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
        let tags = Tag.tag who tags difficulty content.tags in
        let ltags = Tag.folder_as_list tags in
        let active = get_current_notifications ltags content.active in
        { content with tags; active }
      | NewNotification id ->
        let active = Notifications.push content.active id in
        { content with active }
      | HideNotification id ->
        let active = Notifications.remove content.active id in
        let hidden = Notifications.push content.hidden id in
        { content with active; hidden }
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
    | NewNotification id ->
      Printf.sprintf "New notification %s\n" id
    | HideNotification id ->
      Printf.sprintf "Hide notification %s\n" id


end)

let is_teacher      e = observe e (fun u -> return (content u).teacher)
let password_digest e = observe e (fun u -> return (content u).password_digest)
let login           e = observe e (fun u -> return (content u).login)
let firstname       e = observe e (fun u -> return (content u).firstname)
let surname         e = observe e (fun u -> return (content u).surname)
let email           e = observe e (fun u -> return (content u).email)
let active          e = observe e (fun u -> return (content u).active)
let hidden          e = observe e (fun u -> return (content u).hidden)

let fullname e =
  lwt firstname = firstname e in
  lwt surname = surname e in
  return (firstname ^ " " ^ surname)

let user_module = "hackojo.user <here@hackojo.org>"

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

let create_user_account login password =
  let id = user_identifier login in
  let password_digest =
    match password with
      | `Digest d -> d
      | `Text password -> digest password
  in
  lwt status = get_user_info login "status" in
  let teacher = (status = "teacher") in
  lwt firstname = get_user_info login "firstname" in
  lwt surname = get_user_info login "surname" in
  lwt email = get_user_info login "email" in
  let active = get_current_notifications [] Notifications.empty in
  let hidden = Notifications.empty in
  let data = {
    login;
    logged = false;
    password_digest;
    teacher;
    firstname;
    surname;
    email;
    tags = Tag.Set.empty;
    active;
    hidden
  }
  in
  make ~init:(data, empty_dependencies, []) id

let create_admin_account () =
  create_user_account admin_username (`Digest (get_admin_password ()))

let up () = VFS.(
  load_all_notifications ()
  >> if not (exists path) then (
    create user_module path
    >> create_admin_account ()
    >> return (`OK ())
  ) else
    return (`OK ())
)

let shutdown () =
  Printf.eprintf "Saving all notifications\n%!";
  shutdown ();
  Lwt.async save_all_notifications

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

let register login password =
  get_user_info login "exists" >>= function
    | "1" ->
      if login = admin_username then
        return (`KO (`AlreadyExists (path_of_identifier admin_id)))
      else
        create_user_account login (`Text password)
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

let get_notifications_folder filename user = Notifications.(
  resource user filename >>= function
    | `OK (res, _) -> return (`OK (res, read res))
    | `KO e -> return (`KO e)
)

let push_notification uid notification_id =
  make uid >>= function
    | `OK user -> change user (NewNotification notification_id)
    | `KO e -> return () (* FIXME *)

let hide_notification uid notification_id =
  make uid >>= function
    | `OK user -> change user (HideNotification notification_id)
    | `KO e -> return () (* FIXME *)

let get_active_notifications uid =
  make uid >>>= Notifications.(fun user ->
    lwt active = active user in
    lwt now_hidden, still_active =
      Lwt_list.partition_s (to_be_automatically_hidden uid) active
    in
    Lwt_list.iter_s (hide_notification uid) now_hidden
    >> return (`OK still_active)
  )

let notify if_has_tags notification uid =
  push_new_notification if_has_tags notification;
  Lwt_list.for_all_s (has_tag uid) if_has_tags >>= function
    | true -> push_notification uid notification
    | false -> return ()

let notify_all if_has_tags notification =
  all_user_ids () >>= Lwt_list.iter_s (notify if_has_tags notification)

let _ =
  let open AkaInterpreter in
      user_has_tag    := (fun s t -> has_tag (identifier_of_string s) t);
      notify_all_user := notify_all

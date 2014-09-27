open Lwt
open ExtPervasives
open ExtUnix
open Identifier
open Statement
open InMemory
open OnDisk

type notification_message =
  | Message          of statement
  | EphemeralMessage of float * statement
  | GotoExercise     of Identifier.t * statement
deriving (Json)

type notification_hash = string
deriving (Json)

type notification = {
  hash    : notification_hash;
  message : notification_message;
  date    : float;
}
deriving (Json)

type notification_identifier = Resource.name deriving (Json)

module Index = Rb.Dict (struct
  type key   = notification_hash deriving (Json)
  type image = Resource.name list deriving (Json)
  let compare = String.compare
end)

type internal_state = {
  next  : int;
  index : Index.t;
}
deriving (Json)

let add_hash hash name index =
  let current = try Index.lookup hash index with Not_found -> [] in
  Index.update hash (name :: current) index

type public_change =
  | NewNotification of notification

include Entity.Make (struct
  type data = internal_state deriving (Json)
  type change = public_change

  let kind = "notifications"

  let react state mdeps cs later =
    let make_change content = function
      | NewNotification n ->
        let nidx = n.hash in
        let name = string_of_int content.next in
        let filename = resource_real_path (identifier state) name in
        ltry (
          ExtUnix.echo (Deriving_Json.to_string Json.t<notification> n) filename
        ) >>= function
          | `OK _ -> return {
            index = add_hash nidx name content.index;
            next  = content.next + 1
          }
          | `KO _ -> return content (* FIXME *)
    in
    lwt content = Lwt_list.fold_left_s make_change (content state) cs in
    return (UpdateContent content)

  let current_version = "1.0"

  let converters = []

  let string_of_change = function
    | NewNotification _ -> "new notification"

end)

let notifications_id = Identifier.identifier_of_string "notifications"

let notifications = ref None

let up () =
  make notifications_id >>= function
    | `OK e ->
      notifications := Some e;
      return (`OK ())
    | `KO _ ->
      let state = { index = Index.empty; next = 0 } in
      make ~init:(state, empty_dependencies, []) notifications_id
      >>= function
        | `OK e -> notifications := Some e; return (`OK ())
        | _ -> return (`OK ()) (* FIXME *)

let notifications () = match !notifications with
  | None -> assert false (* By system initialization. *)
  | Some e -> e

let load_notification name =
  let file = resource_real_path notifications_id name in
  ltry (cat file) >>>= fun content ->
  try_lwt
    return (`OK (Deriving_Json.from_string Json.t<notification> content))
  with _ -> return (`KO `Serialization)

exception Found of Resource.name
let rec notification_identifier n =
  lwt index = observe (notifications ()) (fun state ->
    return (content state).index
  )
  in
  try_lwt
    let check f = load_notification f >>= function
      | `OK n' ->
        if n.message = n'.message then
          raise_lwt (Found f)
        else
          return ()
      | `KO _ -> return () (* FIXME *)
    in
    let rec search = function
      | [] -> raise_lwt Not_found
      | f :: fs -> check f >> search fs
    in
    search (Index.lookup n.hash index)
  with
    | Not_found ->
      change (notifications ()) (NewNotification n)
      >> Lwt_unix.yield ()
      >> Lwt_unix.sleep 0.1
      >> notification_identifier n
    | Found file ->
      return file

type folder = notification_identifier list
deriving (Json)

let push folder n =
  if List.mem n folder then folder else n :: folder

let remove folder n =
  List.filter (fun n' -> n <> n') folder

let empty =
  []

let read resource =
  Deriving_Json.from_string Json.t<folder> (Resource.content resource)

let write resource folder =
  Resource.set_content resource (Deriving_Json.to_string Json.t<folder> folder)

let user_has_subscribed =
  ref (fun (_ : Identifier.t) (_ : Identifier.t) -> return false)

let message_to_be_automatically_hidden user_id = function
  | Message _ ->
    return false
  | EphemeralMessage (exp, _) ->
    return (Unix.gettimeofday () > exp)
  | GotoExercise (exo_id, _) ->
    !user_has_subscribed user_id exo_id

let to_be_automatically_hidden user_id n =
  load_notification n >>= function
    | `OK n -> message_to_be_automatically_hidden user_id n.message
    | `KO _ -> return false (* FIXME *)

let make_notification message =
  let hash = Digest.string (Marshal.to_string message []) in
  notification_identifier {
    hash;
    message;
    date = Unix.gettimeofday ()
  }

module SR = Statement.ReifyFromAka

let _ =
  let open AkaInterpreter in
   message := (fun s ->
     let msg = Message (SR.statement s) in
     make_notification msg
   );
   goto_exercise := (fun id s ->
     let id = "aka/" ^ Statement.flatten_string SR.(template string id) in
     let msg = GotoExercise (identifier_of_string id, SR.statement s) in
     make_notification msg
   )

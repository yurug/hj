(* -*- tuareg -*- *)

{shared{

open CORE_identifier

(* FIXME: Add a notion of notification expiration? *)
type message =
  | Notification of notification

and notification =
  | EvaluationNeeded of identifier

deriving (Json)

type t = message deriving (Json)

}}

let string_of_notification = function
  | EvaluationNeeded id ->
    I18N.String.evaluation_needed_for_exercise (string_of_identifier id)

let string_of_message = function
  | Notification n -> string_of_notification n

let evaluation_needed id =
  Notification (EvaluationNeeded id)

type status =
  | Read
  | Unread
deriving (Json)

(* FIXME: Use a more efficient representation if needed. *)
type mbox = (message * status) list
deriving (Json)

let empty_mbox () : mbox =
  []

(* FIXME: We assume small mbox. Is that reasonable? *)
let push msg mbox =
  let rec aux = function
    | [] -> [(msg, Unread)]
    | (msg', _) :: mbox when msg = msg' -> (msg, Unread) :: mbox
    | m :: mbox -> m :: (aux mbox)
  in
  aux mbox

let get_messages status mbox =
  fst (List.(split (filter (fun (_, s) -> s = status) mbox)))

let unread mbox = get_messages Unread mbox

let read mbox = get_messages Read mbox

let mark_as_read msg mbox =
  let rec aux = function
    | [] -> []
    | (msg', _) :: mbox when msg = msg' -> (msg, Read) :: mbox
    | m :: mbox -> m :: (aux mbox)
  in
  aux mbox

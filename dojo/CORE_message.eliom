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
type timestamp = float
deriving (Json)

type mbox = (message * status * timestamp) list
deriving (Json)

let empty_mbox () : mbox =
  []

(* FIXME: We assume small mbox. Is that reasonable? *)
let push msg mbox =
  let rec aux = function
    | [] -> [(msg, Unread, Unix.gettimeofday ())]
    | (msg', _, t) :: mbox when msg = msg' -> (msg, Unread, t) :: mbox
    | m :: mbox -> m :: (aux mbox)
  in
  aux mbox

let get_messages status mbox =
  List.(map (fun (m, _, _) -> m) (filter (fun (_, s, _) -> s = status) mbox))

let unread mbox = get_messages Unread mbox

let read mbox = get_messages Read mbox

let mark_as_read msg mbox =
  let rec aux = function
    | [] -> []
    | (msg', _, t) :: mbox when msg = msg' -> (msg, Read, t) :: mbox
    | m :: mbox -> m :: (aux mbox)
  in
  aux mbox

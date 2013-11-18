(** -*- tuareg -*- *)

{shared{

(** Evaluation context. *)

open Lwt

open CORE_entity
open CORE_identifier
open CORE_error_messages
open COMMON_pervasives

type rule =
  | Answer  of string
  | Command of string
  | TimeOut of int
deriving (Json)

type context =
  | Empty
  | Compose of rule * context
deriving (Json)

type t = context deriving (Json)

type criteria = string deriving (Json)

type grade = int * int deriving (Json)

type score = (criteria * grade) list deriving (Json)

let empty = Empty

let push r c = Compose (r, c)

let answer fname = Answer fname

let command c = Command c

let timeout t = TimeOut t

let get what c =
  let rec aux last = function
  | Empty -> last
  | Compose (w, qs) ->
    match what w with
      | None -> aux last qs
      | Some y -> Some y
  in
  aux None c

let get_answer_form = get (function Answer fname -> Some fname | _ -> None)
let get_command = get (function Command c -> Some c | _ -> None)
let get_timeout = get (function TimeOut t -> Some t | _ -> None)

}}

type submission = string deriving (Json)

let new_submission s = s

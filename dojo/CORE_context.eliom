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

let null_score = []

let new_score s ss =
  (* FIXME: Merge scores of the same criteria. *)
  s @ ss

let make_seed () =
  Random.bits ()

let substitute_seed seed cmd = Str.(
  global_replace (regexp "%seed%") (string_of_int seed) cmd
)

let marker_io_interpretation seed line = Str.(
  if string_match
    (regexp "SCORE \\([0-9]+\\) \\([^,]\\) \\([0-9]+\\)/\\([0-9+]\\)")
    line 0
  then
    let seed' = int_of_string (matched_group 1 line) in
    if seed <> seed' then
      None
    else Some [(matched_group 2 line,
                (int_of_string (matched_group 3 line),
                 int_of_string (matched_group 4 line)))]
  else
    None
)

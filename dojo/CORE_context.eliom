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
  | Choices of string list
  | KeyValues of string list
  | ExpectedValues of string list
  | ExpectedChoices of int list
  | Command of string
  | TimeOut of int
deriving (Json)

type context =
  | Empty
  | Compose of rule * context
deriving (Json)

type t = context deriving (Json)

let rec string_of_context = Printf.(function
  | Empty -> ""
  | Compose (ExpectedValues vs, c) ->
    sprintf "expected(%s) %s"
      (String.concat ", " vs)
      (string_of_context c)
  | Compose (ExpectedChoices vs, c) ->
    sprintf "valid_choices(%s) %s"
      (String.concat ", " (List.map string_of_int vs))
      (string_of_context c)
  | Compose (KeyValues vs, c) ->
    sprintf "keys(%s) %s"
      (String.concat ", " vs)
      (string_of_context c)
  | Compose (Choices vs, c) ->
    sprintf "choices(%s) %s"
      (String.concat ", " vs)
      (string_of_context c)
  | Compose (Answer f, c) -> sprintf "file(%s) %s" f (string_of_context c)
  | Compose (Command d, c) -> sprintf "cmd(%s) %s" d (string_of_context c)
  | Compose (TimeOut t, c) -> sprintf "timeout(%d) %s" t (string_of_context c)
)

type criteria = string deriving (Json)

type grade = int * int deriving (Json)

type score = (criteria * grade) list deriving (Json)

let string_of_score score =
  String.concat " " (List.map (fun (criteria, (n, o)) ->
    Printf.sprintf "%s [%d/%d]" criteria n o
  ) score)

let empty = Empty

let push r c = Compose (r, c)

let answer fname = Answer fname

let choices cs = Choices cs

let key_values keys = KeyValues keys

let expected_values kvs = ExpectedValues kvs

let expected_choices cs = ExpectedChoices cs

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

let all what c =
  let rec aux last = function
  | Empty -> last
  | Compose (w, qs) ->
    match what w with
      | None -> aux last qs
      | Some y -> aux (y :: last) qs
  in
  aux [] c

let get_answer_form = get (function
  | Answer fname -> Some (`Filename fname)
  | KeyValues keys -> Some (`KeyValues keys)
  | Choices cs -> Some (`Choices cs)
  | _ -> None)

let get_command = get (function
  | Command c -> Some (`Command c)
  | ExpectedValues kvs -> Some (`ExpectedValues kvs)
  | ExpectedChoices cs -> Some (`ExpectedChoices cs)
  | _ -> None)

let get_timeout = get (function TimeOut t -> Some t | _ -> None)

type submission =
  | SubmittedFile of string * string
  | SubmittedValues of string list
  | SubmittedChoices of int list
deriving (Json)

}}


let equivalent_submission s1 s2 =
  match s1, s2 with
    | SubmittedFile (f1, digest1), SubmittedFile (f2, digest2) ->
      f1 = f2 && digest1 = digest2
    | s1, s2 ->
      s1 = s2

let equivalent_context c1 c2 =
  c1 = c2

let string_of_submission = function
  | SubmittedFile (f, digest) ->
    Printf.sprintf "file(%s[%s])" f digest
  | SubmittedValues vs ->
    Printf.sprintf "values(%s)" (String.concat "," vs)
  | SubmittedChoices vs ->
    Printf.sprintf "choices(%s)" (String.concat "," (List.map string_of_int vs))

let new_submitted_file s =
  let d = try Digest.file s with _ -> "NoFileNoDigest" in
  SubmittedFile (s, d)

let new_submitted_values vs = SubmittedValues vs

let new_submitted_choices cs = SubmittedChoices cs

let null_score = []

(* FIXME: Factorize the following two kinds of answers? *)
let check_expected_values xs = function
  | SubmittedValues vs ->
    let score =
      if List.length vs <> List.length xs then
        (0, List.length xs)
      else
        List.fold_left2 (fun (m, o) v x ->
          if v = x then (succ m, succ o) else (m, succ o)
        ) (0, 0) xs vs
    in
    [("Score", score)]
  | _ ->
    null_score

let check_expected_choices xs = function
  | SubmittedChoices cs ->
    let score =
      if List.length cs <> List.length xs then
        (0, 0)
      else
        if List.(for_all (fun x -> mem x xs) cs) then
          (1, 1)
        else
          (0, 1)
    in
    [("Score", score)]
  | _ ->
    null_score

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

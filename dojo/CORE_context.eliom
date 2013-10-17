(** -*- tuareg -*- *)

{shared{

(** Evaluation context. *)

open Lwt

open CORE_entity
open CORE_identifier
open CORE_error_messages
open COMMON_pervasives

type rule =
  | Answer of string
deriving (Json)

type context =
  | Empty
  | Compose of rule * context
deriving (Json)

type criteria = string deriving (Json)

type grade = int * int deriving (Json)

type score = (criteria * grade) list deriving (Json)

let empty = Empty

let push r c = Compose (r, c)

let answer fname = Answer fname

let get_answer_form c =
  let rec aux last = function
  | Empty -> last
  | Compose (Answer fname, qs) -> aux (Some (`Filename fname)) qs
  in
  aux None c

}}

type job =
  | ExecutableJob of CORE_machine.command_job
deriving (Json)

(** -*- tuareg -*- *)

(** Evaluation context. *)

{shared{

type context deriving (Json)

type rule deriving (Json)

type criteria = string deriving (Json)

type grade = int * int deriving (Json)

type score = (criteria * grade) list deriving (Json)

val empty : context

val push : rule -> context -> context

val answer : string -> rule

val get_answer_form : context -> [> `Filename of string ] option

}}

type job deriving (Json)

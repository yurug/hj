(** -*- tuareg -*- *)

(** Evaluation context. *)

{shared{

type context deriving (Json)

type rule deriving (Json)

val empty : context

val push : rule -> context -> context

val answer : string -> rule

val get_answer_form : context -> [> `Filename of string ] option

}}

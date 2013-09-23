(* -*- tuareg -*- *)

(** Parsers for the description format. *)
{shared{

val questions_of_string :
  string ->
  [> `KO of
      [> `SyntaxError of Lexing.position * Lexing.position * string
      | `SystemError of string ]
  | `OK of CORE_description_CST.questions ]

}}

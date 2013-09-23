(* -*- tuareg -*- *)

(** Parsers for the description format. *)
{shared{

  open CORE_description_CST

  val questions_of_string :
    string ->
    [> `KO of
        [> `SyntaxError of Lexing.position * Lexing.position * string
        | `SystemError of string ]
    | `OK of questions with_raw
    ]

}}

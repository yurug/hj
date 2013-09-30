(* -*- tuareg -*- *)

(** Parsers for the description format. *)
{shared{

  open CORE_description_CST

  val exercise_of_string :
    string ->
    [> `KO of
        [> `SyntaxError of Lexing.position * Lexing.position * string
        | `SystemError of string ]
    | `OK of exercise with_raw
    ]

}}

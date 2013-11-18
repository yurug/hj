(* -*- tuareg -*- *)

(** Parsers for the description format. *)
{shared{

  open CORE_description_CST

  val exercise_of_string :
    string ->
    [> `KO of
        [> `SyntaxError of CORE_errors.position * CORE_errors.position * string
        | `SystemError of string ]
    | `OK of exercise with_raw
    ]

}}

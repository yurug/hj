(* -*- tuareg -*- *)

(** Parsers for the description format. *)
{shared{

  open CORE_description_CST

  val exercise_of_string :
    string ->
    [> `KO of [ CORE_errors.all ]
    | `OK of exercise with_raw
    ]

}}

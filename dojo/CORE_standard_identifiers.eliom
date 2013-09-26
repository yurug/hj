(* -*- tuareg -*- *)

(** Standard identifiers. *)

open CORE_identifier

let root relative p =
  if relative then
    concat (path_of_string CORE_config.ressource_root) p
  else
    p

let tests_path = from_strings [ "tests" ]
let users_path = from_strings [ "users" ]
let system_path = from_strings [ "system" ]
let exercises_path = from_strings [ "exercises" ]
let questions_path = from_strings [ "questions" ]

let std_paths = [ tests_path; users_path; system_path ]

let fresh =
  let r = ref 0 in
  fun path p ->
    incr r;
    identifier_of_path (concat path (from_strings [ p ^ string_of_int !r ]))

let assigner =
  identifier_of_path (
    concat system_path (CORE_identifier.make [ label "assigner" ])
  )

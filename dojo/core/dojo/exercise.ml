open Lwt
open Entity
open InMemory
open Identifier
open ExtPervasives
open ExtProcess

let path = Identifier.from_strings [ "exercise" ]

let exercise_module = "hackojo.exercise <here@hackojo.org>"

let up () = VFS.(
  if not (exists path) then
    create exercise_module path
  else
    return (`OK ())
)

let exercise_identifier name =
  Identifier.(identifier_of_path (concat path (path_of_string name)))

type internal_state = {
  contributors : identifier list
} deriving (Json)

include Entity.Make (struct

  type data = internal_state deriving (Json)

  type change = unit

  let react state mdeps cs later =
    return NoUpdate

  let current_version = "1.0"

  let converters = []

  let string_of_change () = ""

end)

let create who name =
  let id = exercise_identifier name in
  make id >>= function
    | `OK _ -> return (`KO (`AlreadyExists (path_of_identifier id)))
    | `KO _ ->
      User.is_teacher who >>= function
        | true ->
          let data = { contributors = [ User.identifier who ] } in
          let init = (data, empty_dependencies, []) in
          make ~init id
        | false ->
          return (`KO `StudentsCannotCreateExercise)

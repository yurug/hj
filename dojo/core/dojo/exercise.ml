open Lwt
open Entity
open InMemory
open Identifier
open ExtPervasives
open ExtProcess

let path = Identifier.from_strings [ "exercise" ]

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


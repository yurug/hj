(* -*- tuareg -*- *)

open Lwt

open CORE_exercise
open CORE_error_messages

open TXT_services
open TXT_utils

let _update =
  command register_update update update_fallback
    (fun e _ _ ->
      Eliom_reference.get e >>= function
        | `OK _ -> return "Update pushed."
        | `KO e -> return (string_of_error e)
    )

(* -*- tuareg -*- *)

open Lwt
open ExtPervasives

let focus_eref =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    ~persistent:("focuses")
    []

let get_focus = server_function Json.t<string> (
  fun exo_str ->
    lwt list = Eliom_reference.get focus_eref in
    try_lwt
      return (Some (List.assoc exo_str list))
    with Not_found ->
      return None
)

let save_focus = server_function Json.t<string * string> (fun (exo_str, name) ->
  lwt focuses = Eliom_reference.get focus_eref in
  Eliom_reference.set focus_eref (update_assoc exo_str name focuses)
)

(* -*- tuareg -*- *)

open Lwt
open Eliom_service.Http

type error_state = [ `Success | `Error of string ]

let error_state =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    ~persistent:"error_state"
    (`Success : error_state)

let set e = Eliom_reference.set error_state (`Error e)

let return_txt s = Lwt.return (s ^ "\n", "text/plain")

let msg s = "error:" ^ s

let fallback s =
  Eliom_registration.String.register_service
    ~https:true
    ~path:[s] ~get_params:Eliom_parameter.unit (fun () () ->
      Eliom_reference.get error_state >>= function
        | `Success -> return_txt "you_should_not_see_that"
        | `Error m -> return_txt (msg m)
    )

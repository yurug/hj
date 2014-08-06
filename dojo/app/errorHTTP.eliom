(* -*- tuareg -*- *)

open Eliom_service.Http

let return_txt s = Lwt.return (s ^ "\n", "text/plain")

let fallback s =
  Eliom_registration.String.register_service
    ~https:true
    ~path:[s] ~get_params:Eliom_parameter.unit (fun () () ->
    return_txt "Invalid argument."
  )

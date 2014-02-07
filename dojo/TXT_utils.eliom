(* -*- tuareg -*- *)

open Lwt

let return_txt s = Lwt.return (s ^ "\n", "text/plain")

let register_simple_service ~service f =
  Eliom_registration.String.register ~service (fun g p -> f g p >>= return_txt)

let command register service fallback response =
  let r = Eliom_reference.eref ~scope:Eliom_common.default_session_scope (`OK ()) in
  register r ~service;
  register_simple_service ~service:fallback (response r)

let register_request what how =
  Eliom_registration.String.register_service
    ~path:[what]
    ~get_params:Eliom_parameter.unit
    (fun _ _ -> how ())

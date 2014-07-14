(** -*- tuareg -*- *)

open Eliom_service.Http

open CORE_user
open CORE_exercise

let login_fallback = service ~path:["login"] ~get_params:Eliom_parameter.unit ()
let login          = login_service ~fallback:login_fallback

let logout_fallback = service ~path:["logout"] ~get_params:Eliom_parameter.unit ()
let logout          = logout_service' ~fallback:logout_fallback

let update_fallback = service ~path:["update"] ~get_params:Eliom_parameter.unit ()
let update          = update_service ~fallback:update_fallback

(* -*- tuareg -*- *)

(** Declaration of HTTP_services. *)

(** The  services of  the plateform call  each others in  an arbitrary
    way. As a consequence, they must be declared first. *)

open Lwt
open Eliom_service
open Eliom_parameter

(** [redirect_service s x] installs a redirection service to service
    [s] applied to [x] that acts as a fallback for POST services. In
    Ocsigen terminology, a fallback is a service that provides an URL
    for POST services and is used when the related POST service is
    incorrectly used.
    @see <http://ocsigen.org/eliom/manual/server-services> Ocsigen manual. *)
let redirect_service s x =
  Eliom_registration.Redirection.register_service
    ~path:["redir"]
    ~get_params:unit
    (fun () () -> return (preapply s x))

let conditional_redirect_service default ls rs lparams =
  let r =
    Eliom_reference.eref ~scope:Eliom_common.default_process_scope default
  in
  (Eliom_registration.Redirection.register_service
    ~path:["redir"]
    ~get_params:lparams
    (fun ps () ->
      Eliom_reference.get r >>= function
        | `Left _ -> return (preapply ls ps)
        | `Right d -> return (preapply rs d)
    ),
   (Eliom_reference.set r),
   (fun () -> Eliom_reference.get r))

(** The root service. *)
let root = service ~path:[] ~get_params:unit ()

(** The autotest service. *)
let autotest = service ~path:["autotest"] ~get_params:unit ()

let login     = CORE_user.login_service ~fallback:root
let logout    = CORE_user.logout_service ~fallback:root

let subscribe_form = service ~path:["subscribe"] ~get_params:unit ()

let subscribe and_then =
  let fallback, decide, result =
    conditional_redirect_service (`Left None) subscribe_form and_then unit
  in
  (CORE_user.subscribe_service ~fallback, decide, result)

(** [page_of]  is the service that serves the  HTML page of entities.
    The GET  parameters are encoded  in a suffix  of the
    URL.  For  instance,  the  page  of  "users/luke"  is  located  at
    [users/luke]. *)
let page_of =
  service ~path:[] ~get_params:(suffix (list "id" (string "label"))) ()

let about = service ~path:["about"] ~get_params:unit ()

let create_form = service ~path:["create"] ~get_params:(string "kind") ()

let create creation_service and_then =
  let fallback, decide, result =
    conditional_redirect_service
      (`Left None)
      create_form
      and_then
      (string "kind")
  in
  (creation_service ~fallback, decide, result)

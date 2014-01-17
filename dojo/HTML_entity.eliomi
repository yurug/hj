(* -*- tuareg -*- *)

(** HTML pages for entity. *)

open Lwt
open Eliom_content.Html5.D
open Html5_types

open CORE_entity
open CORE_identifier

(** [register_page_maker detect retrieve] publishes a way to
    [detect] if some identifier [id] can have its page using
    [retrieve id]. *)
val register_page_maker:
  (identifier -> bool) ->
  (identifier -> [ body_content ] elt Lwt.t) ->
  unit

val offer_creation :
  (CORE_identifier.identifier ->
   [< `KO of [< CORE_errors.all > `UndefinedEntity ] | `OK of 'a ] Lwt.t) ->
  (('b ->
    (unit, unit,
     [> `Attached of
         ([> `Internal of [> `Service ] ], [> `Get ]) Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit,
     [< Eliom_service.registrable > `Unregistrable ], 'c)
      Eliom_service.service) ->
   ('d ->
    (unit, unit,
     [> `Attached of
         ([> `Internal of [> `Service ] ], [> `Get ]) Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit,
     [< Eliom_service.registrable > `Unregistrable ], 'e)
      Eliom_service.service) ->
   (string list, unit, [< Eliom_service.get_service_kind ],
    [< Eliom_service.suff ], 'f, unit, [< Eliom_service.registrable ], 'g)
     Eliom_service.service) ->
  ('a -> ([> Html5_types.div ] as 'h) Eliom_content.Html5.D.elt Lwt.t) ->
  CORE_identifier.identifier -> 'h Eliom_content.Html5.D.elt Lwt.t

val reactive_div :
  CORE_entity.some_t list
  -> (unit -> unit) client_value option
  -> (unit -> 'a Lwt.t)
  -> ('a -> [ body_content ] elt list Lwt.t) client_value
  -> [ body_content ] elt Lwt.t

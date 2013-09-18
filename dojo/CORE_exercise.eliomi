(** -*- tuareg -*- *)

(** Exercise entities. *)

include CORE_entity.S

type assignment_kind = [ `Must | `Should | `Can | `Cannot ]

(** [assignment_rule e k] returns the assignment rule of the
    exercise [e] for the assignment kind [k]. *)
val assignment_rule : t -> assignment_kind -> CORE_property.rule Lwt.t

val create_service :
  (t ->
   (unit, unit, Eliom_service.get_service_kind, [ `WithoutSuffix ], unit,
    unit, Eliom_service.registrable, 'a)
     Eliom_service.service) ->
  (string ->
   (unit, unit, Eliom_service.get_service_kind, [ `WithoutSuffix ], unit,
    unit, Eliom_service.registrable, 'a)
     Eliom_service.service) ->
  (string list, unit,
   [> `Attached of
       ([> `Internal of [> `Service ] ], [> `Get ]) Eliom_service.a_s ],
   [ `WithSuffix ],
   [ `One of string ] Eliom_parameter.param_name Eliom_parameter.listnames,
   unit, [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

(** -*- tuareg -*- *)

(** Exercise entities. *)

{shared{
type description
}}

include CORE_entity.S with type data = description

{client{
type data = description
}}

type assignment_kind = [ `Must | `Should | `Can | `Cannot ]


(** [assignment_rule e k] returns the assignment rule of the
    exercise [e] for the assignment kind [k]. *)
val assignment_rule : t -> assignment_kind -> CORE_property.rule Lwt.t

open CORE_description_CST

type patch = position * position * string

val change_from_user_description
  : t -> questions with_raw -> [
  | `OK of (CORE_identifier.t * question_definition) list
  | `KO of [
    | `UndefinedEntity of CORE_identifier.t
    | `NeedPatch       of patch
    | `AlreadyExists   of CORE_identifier.path
    | `SystemError     of string
  ]] Lwt.t

val raw_user_description_source : t -> CORE_source.t Lwt.t

{client{
val raw_user_description : CORE_identifier.t -> string Lwt.t
}}

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

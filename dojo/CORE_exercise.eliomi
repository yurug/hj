(** -*- tuareg -*- *)

(** Exercise entities. *)

{shared{
type composer = Par | Seq deriving (Json)

type checkpoint = string deriving (Json)

type questions =
  | Compose           of composer * questions list
  | Statement         of string * questions
  | ContextRule       of CORE_context.rule * questions
  | Checkpoint        of checkpoint * questions
  | Sub               of CORE_identifier.t * CORE_entity.timestamp
 deriving (Json)

type description
}}

include CORE_entity.S with type data = description

{client{
type data = description
}}

type assignment_kind = [ `Must | `Should | `Can | `Cannot ]

{shared{
(** [title e] returns the title of [e]. *)
val title : description -> string

(** [questions e] returns the questions of [e]. *)
val questions : description -> questions
}}

(** [assignment_rule e k] returns the assignment rule of the
    exercise [e] for the assignment kind [k]. *)
val assignment_rule : t -> assignment_kind -> CORE_property.rule Lwt.t

(** [all_checkpoints e] returns all the checkpoints in the description
    of [e]. *)
val all_checkpoints : t -> checkpoint list Lwt.t

(** [context_of_checkpoint e c] computes the context of [c] in [e]. *)
val context_of_checkpoint : t -> checkpoint -> CORE_context.t Lwt.t

open CORE_description_CST

type patch = position * position * string

val make_blank : CORE_identifier.t -> [ `OK of t
    | `KO of [>
      | `UndefinedEntity of CORE_identifier.t
      | `AlreadyExists   of CORE_identifier.path
      | `SystemError     of string
    ]] Lwt.t

val change_from_user_description
  : t -> exercise with_raw -> [
  | `OK of (CORE_identifier.t * exercise with_raw) list
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

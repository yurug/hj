(** -*- tuareg -*- *)

(** Exercise entities. *)

{shared{

type checkpoint = CORE_questions.checkpoint deriving (Json)

val string_of_checkpoint : checkpoint -> string

type questions = CORE_questions.t deriving (Json)

type description

}}

val answer_of_dependency_kind : string

type public_change =
  | NewAnswer of
      CORE_identifier.t list (** Authors *)
    * CORE_identifier.t      (** Answer  *)
  | EvalQuestions of CORE_identifier.t list
  | Update of questions
  | UpdateSource of CORE_description_CST.exercise CORE_description_CST.with_raw

include CORE_entity.S
with type data = description and type change = public_change

{client{
type data = description
}}

type assignment_kind = [ `Must | `Should | `Can | `Cannot ]

{shared{
(** [title e] returns the title of [e]. *)
val title : description -> string

(** [questions e] returns the questions of [e]. *)
val questions : description -> questions

(** [current_value e] returns the current evaluation of the exercise
    description as a list of questions. *)
val current_value : description -> CORE_questions.questions_result option
}}

(** [assignment_rule e k] returns the assignment rule of the
    exercise [e] for the assignment kind [k]. *)
val assignment_rule : t -> assignment_kind -> CORE_property.rule Lwt.t

(** [all_checkpoints e] returns all the checkpoints in the description
    of [e]. *)
val all_checkpoints
  : t -> CORE_identifier.t list -> checkpoint list Lwt.t

(** [context_of_checkpoint e c] computes the context of [c] in [e]. *)
val context_of_checkpoint
  : t -> checkpoint -> CORE_identifier.t list -> CORE_context.t option Lwt.t

(** [eval e] forces the evaluation of [e]. *)
val eval
  : t -> CORE_identifier.t list -> unit Lwt.t

(** [eval_if_needed e] compute the evaluation of [e] if needed. *)
val eval_if_needed
  : t -> CORE_identifier.t list -> CORE_questions.questions_result option Lwt.t

val raw_user_description_source: CORE_identifier.t -> CORE_source.t Lwt.t

type patch =
    CORE_errors.position * CORE_errors.position * string

val make_blank
: CORE_identifier.t -> [ `OK of t
                       | `KO of [>
                                | `UndefinedEntity of CORE_identifier.t
                                | `AlreadyExists   of CORE_identifier.path
                                | `SystemError     of string
                                ]] Lwt.t

val change_from_user_description
  : t -> CORE_description_CST.exercise CORE_description_CST.with_raw ->
  unit Lwt.t

(* FIXME: recover this old rich return type [
  | `OK of (CORE_identifier.t
            * CORE_description_CST.exercise CORE_description_CST.with_raw) list
  | `KO of [
    | `UndefinedEntity of CORE_identifier.t
    | `NeedPatch       of patch
    | `AlreadyExists   of CORE_identifier.path
    | `SystemError     of string
  ]] Lwt.t
*)

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

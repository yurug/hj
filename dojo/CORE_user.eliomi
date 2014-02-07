(* -*- tuareg -*- *)

(** User entities. *)

include CORE_entity.S

(** [authenticate login passworddigest] return an authenticated user
    entity. *)
val authenticate : string -> string ->
  [ `OK of t
  | `KO of [
    | `MaximalNumberOfLoginAttemptsReached
    | `BadLoginPasswordPair
    | `SystemError           of string
  ]
  ] Lwt.t

(** A cookie can certify that a user is already logged. *)
val logged_user : unit -> [ `NotLogged | `FailedLogin | `Logged of t ] Lwt.t

(** [login u] returns the login of the user. *)
val login : t -> string Lwt.t

(** [firstname u] returns the firstname of the user. *)
val firstname : t -> string Lwt.t

(** [surname u] returns the surname of the user. *)
val surname : t -> string Lwt.t

(** [last_connection u] returns a string representing the date of the
    last connection of the user [u]. *)
val last_connection : t -> string Lwt.t

(** [is_teacher u] returns [true] if [u] is a teacher. *)
val is_teacher : t -> bool Lwt.t

(** [email u] returns the email of the user [u]. *)
val email : t -> string Lwt.t
val set_email : t -> string-> unit Lwt.t

val change_properties
  : t -> (CORE_property.set -> CORE_property.set) -> unit Lwt.t

val has_property
  : t -> CORE_property.t -> bool Lwt.t

val properties
  : t -> CORE_property.set Lwt.t

val send
  : t -> CORE_message.t -> unit Lwt.t

val unread
  : t -> CORE_message.t list Lwt.t

val read
  : t -> CORE_message.t list Lwt.t

val mark_as_read
  : t -> CORE_message.t -> unit Lwt.t

(** Services (automatically inferred using eliomc -i.) *)

val login_service :
  fallback:('a, unit,
              [ `Attached of
                  ([ `Internal of [ `Coservice | `Service ] ], [ `Get ])
                  Eliom_service.a_s ],
              [< Eliom_service.suff ] as 'b, 'c, unit, [< `Registrable ],
              [< Eliom_service.non_ocaml_service ])
             Eliom_service.service ->
    ('a, string * string,
     [> `Attached of
          ([> `Internal of [ `Coservice | `Service ] ], [> `Post ])
          Eliom_service.a_s ],
     'b, 'c,
     [ `One of string ] Eliom_parameter.param_name *
     [ `One of string ] Eliom_parameter.param_name,
     [< Eliom_service.registrable > `Registrable ],
     [> Eliom_service.http_service ])
    Eliom_service.service

val register_login :
  _ ->
  service:(unit, string * string, [< Eliom_service.internal_service_kind ],
           [< Eliom_service.suff ], 'a, 'b, [ `Registrable ],
           Eliom_registration.http_service)
  Eliom_service.service ->
  unit

val logout_service' :
    fallback:('a, unit,
              [ `Attached of
                  ([ `Internal of [ `Coservice | `Service ] ], [ `Get ])
                  Eliom_service.a_s ],
              [< Eliom_service.suff ] as 'b, 'c, unit, [< `Registrable ],
              [< Eliom_service.non_ocaml_service ])
             Eliom_service.service ->
    ('a, unit,
     [> `Attached of
          ([> `Internal of [ `Coservice | `Service ] ], [> `Post ])
          Eliom_service.a_s ],
     'b, 'c, unit, [< Eliom_service.registrable > `Registrable ],
     [> Eliom_service.http_service ])
    Eliom_service.service

val logout_service :
  fallback:(unit, unit,
            [ `Attached of
                ([ `Internal of [ `Service ] ], [ `Get ]) Eliom_service.a_s ],
            [ `WithoutSuffix ], unit, unit, [< Eliom_service.registrable ],
            [< Eliom_service.non_ocaml_service ])
  Eliom_service.service ->
  (unit, unit,
   [> `Attached of
       ([> `Internal of [> `Coservice ] ], [> `Get ]) Eliom_service.a_s ],
   [ `WithoutSuffix ], unit, unit,
   [< Eliom_service.registrable > `Registrable ],
   [> Eliom_service.http_service ])
    Eliom_service.service

val register_logout :
  _ ->
  service:(unit, unit, [< Eliom_service.internal_service_kind ],
           [< Eliom_service.suff ], 'a, 'b, [ `Registrable ],
           Eliom_registration.http_service)
  Eliom_service.service ->
  unit

val subscribe_service :
    fallback:('a, unit,
              [ `Attached of
                  ([ `Internal of [ `Coservice | `Service ] ], [ `Get ])
                  Eliom_service.a_s ],
              [< Eliom_service.suff ] as 'b, 'c, unit, [< `Registrable ],
              [< Eliom_service.non_ocaml_service ])
             Eliom_service.service ->
    ('a, string * (string * (string * (string * string))),
     [> `Attached of
          ([> `Internal of [ `Coservice | `Service ] ], [> `Post ])
          Eliom_service.a_s ],
     'b, 'c,
     [ `One of string ] Eliom_parameter.param_name *
     ([ `One of string ] Eliom_parameter.param_name *
      ([ `One of string ] Eliom_parameter.param_name *
       ([ `One of string ] Eliom_parameter.param_name *
        [ `One of string ] Eliom_parameter.param_name))),
     [< Eliom_service.registrable > `Registrable ],
     [> Eliom_service.http_service ])
    Eliom_service.service


val register_subscribe :
  ([> `Left of string option | `Right of unit ] -> unit Lwt.t) ->
  service:(unit,
           string * (string * (string * (string * string))),
           [< Eliom_service.internal_service_kind ],
           [< Eliom_service.suff ], 'a, 'b, [ `Registrable ],
           Eliom_registration.http_service)
    Eliom_service.service ->
  unit

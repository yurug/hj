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

(** Services (automatically inferred using eliomc -i.) *)

val login_service :
  fallback:('a, unit,
            [ `Attached of
                ([ `Internal of [ `Coservice | `Service ] ], [ `Get ])
                  Eliom_service.a_s ],
            [< Eliom_service.suff ] as 'b, 'c, unit, [< `Registrable ], 'd)
  Eliom_service.service ->
  ('a, string * string,
   [> `Attached of
       ([> `Internal of [ `Coservice | `Service ] ], [> `Post ])
         Eliom_service.a_s ],
   'b, 'c,
   [ `One of string ] Eliom_parameter.param_name *
     [ `One of string ] Eliom_parameter.param_name,
   [< Eliom_service.registrable > `Registrable ], 'e)
    Eliom_service.service

val register_login :
  service:(unit, string * string, [< Eliom_service.internal_service_kind ],
           [< Eliom_service.suff ], 'a, 'b, [ `Registrable ],
           Eliom_registration.http_service)
  Eliom_service.service ->
  unit

val logout_service :
   fallback:(unit, unit,
              [ `Attached of
                  ([ `Internal of [ `Service ] ], [ `Get ]) Eliom_service.a_s ],
              [ `WithoutSuffix ], unit, unit, [< Eliom_service.registrable ],
              'a)
             Eliom_service.service ->
    (unit, unit,
     [> `Attached of
          ([> `Internal of [> `Coservice ] ], [> `Get ]) Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit,
     [< Eliom_service.registrable > `Registrable ], 'b)
    Eliom_service.service

val register_logout :
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
            [< Eliom_service.suff ] as 'b, 'c, unit, [< `Registrable ], 'd)
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
   [< Eliom_service.registrable > `Registrable ], 'e)
    Eliom_service.service

val register_subscribe :
  service:(unit, string * (string * ('a * (string * string))),
           [< Eliom_service.internal_service_kind ], [< Eliom_service.suff ],
           'b, 'c, [ `Registrable ], Eliom_registration.http_service)
  Eliom_service.service ->
  unit

(* -*- tuareg -*- *)

(** Declaration of HTTP_services. *)

(** The  services of  the plateform call  each others in  an arbitrary
    way. As a consequence, they must be declared first. *)

(** The root service. *)
val root :
  (unit, unit,
   [> `Attached of
       ([> `Internal of [> `Service ] ], [> `Get ]) Eliom_service.a_s ],
   [ `WithoutSuffix ], unit, unit,
   [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

(** The about service. *)
val about :
  (unit, unit,
   [> `Attached of
       ([> `Internal of [> `Service ] ], [> `Get ]) Eliom_service.a_s ],
   [ `WithoutSuffix ], unit, unit,
   [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

(** The login service. *)
val login :
  (unit, string * string,
   [> `Attached of
       ([> `Internal of [ `Coservice | `Service ] ], [> `Post ])
         Eliom_service.a_s ],
   [ `WithoutSuffix ], unit,
   [ `One of string ] Eliom_parameter.param_name *
     [ `One of string ] Eliom_parameter.param_name,
   [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

(** The logout service. *)
val logout :
  (unit, unit,
   [> `Attached of
       ([> `Internal of [ `Coservice | `Service ] ], [> `Get ])
         Eliom_service.a_s ],
   [ `WithoutSuffix ], unit, unit,
   [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

(** The subscription form and service. *)
val subscribe :
  ('a, unit, Eliom_service.get_service_kind,
   [< Eliom_service.suff ], 'b, unit,
   [< Eliom_service.registrable ], 'c)
           Eliom_service.service ->
  (unit, string * (string * (string * (string * string))),
   [> `Attached of
       ([> `Internal of [ `Coservice | `Service ] ], [> `Post ])
         Eliom_service.a_s ],
   [ `WithoutSuffix ], unit,
   [ `One of string ] Eliom_parameter.param_name *
     ([ `One of string ] Eliom_parameter.param_name *
         ([ `One of string ] Eliom_parameter.param_name *
             ([ `One of string ] Eliom_parameter.param_name *
                 [ `One of string ] Eliom_parameter.param_name))),
   [< Eliom_service.registrable > `Registrable ], 'd)
    Eliom_service.service *
    ([< `Left of string option | `Right of 'a > `Left ] -> unit Lwt.t)

val subscribe_form :
  (string option, unit,
   [> `Attached of
       ([> `Internal of [> `Service ] ], [> `Get ]) Eliom_service.a_s ],
   [ `WithoutSuffix ], [ `One of string ] Eliom_parameter.param_name,
   unit, [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

(** The autotest service. *)
val autotest :
  (unit, unit,
   [> `Attached of
       ([> `Internal of [> `Service ] ], [> `Get ]) Eliom_service.a_s ],
   [ `WithoutSuffix ], unit, unit,
   [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

(** [redirect_service s x] installs a redirection service to service
    [s] applied to [x] that acts as a fallback for POST services. *)
val redirect_service :
  ('a, unit, Eliom_service.get_service_kind, [< Eliom_service.suff ], 'b,
   unit, [< Eliom_service.registrable ], 'c)
    Eliom_service.service ->
  'a ->
  (unit, unit,
   [> `Attached of
       ([> `Internal of [> `Service ] ], [> `Get ]) Eliom_service.a_s ],
   [ `WithoutSuffix ], unit, unit,
   [< Eliom_service.registrable > `Registrable ], 'c)
    Eliom_service.service

(** *)
val conditional_redirect_service :
  ([< `Left of 'b | `Right of 'c ] as 'a) ->
  ('b, unit, Eliom_service.get_service_kind, [< Eliom_service.suff ], 'd,
   unit, [< Eliom_service.registrable ], 'e)
    Eliom_service.service ->
  ('c, unit, Eliom_service.get_service_kind, [< Eliom_service.suff ], 'f,
   unit, [< Eliom_service.registrable ], 'e)
    Eliom_service.service ->
  (unit, unit,
   [> `Attached of
       ([> `Internal of [> `Service ] ], [> `Get ]) Eliom_service.a_s ],
   [ `WithoutSuffix ], unit, unit,
   [< Eliom_service.registrable > `Registrable ], 'e)
    Eliom_service.service * ('a -> unit Lwt.t)


(** [page_of] is a service to get entities pages. *)
val page_of :
  (string list, unit,
   [> `Attached of
       ([> `Internal of [> `Service ] ], [> `Get ]) Eliom_service.a_s ],
   [ `WithSuffix ],
   [ `One of string ] Eliom_parameter.param_name Eliom_parameter.listnames,
   unit, [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

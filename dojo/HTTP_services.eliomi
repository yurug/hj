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
       ([> `Internal of [ `Coservice | `Service ] ], [> `Post ])
         Eliom_service.a_s ],
   [ `WithoutSuffix ], unit, unit,
   [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

(** The subscription form and service. *)
val subscribe :
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
   [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

val subscribe_form :
  (unit, unit,
   [> `Attached of
       ([> `Internal of [> `Service ] ], [> `Get ]) Eliom_service.a_s ],
   [ `WithoutSuffix ], unit, unit,
   [< Eliom_service.registrable > `Registrable ], 'a)
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

(** [page_of] is a service to get entities pages. *)
val page_of :
  (string list, unit,
   [> `Attached of
       ([> `Internal of [> `Service ] ], [> `Get ]) Eliom_service.a_s ],
   [ `WithSuffix ],
   [ `One of string ] Eliom_parameter.param_name Eliom_parameter.listnames,
   unit, [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

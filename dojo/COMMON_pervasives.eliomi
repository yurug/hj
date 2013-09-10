(* -*- tuareg -*- *)

(** Extension to the OCaml library. *)

{shared{

(** A value of type ['a only] is isomorphic to a
    value of type ['a]. This type constructor is introduced
    to help the type-checker distinguish between a product
    of length 1 and a standard value even if OCaml does not
    make such a distinction. *)
type 'a only = Only of 'a

(** The functor of finite vectors of heterogeneous types.

    The following module defines a type and operations
    over vectors whose shapes are known at compile-time.
*)
module type MapProduct_sig = sig

  (** ['a t] is the type of vector cells whose content has
      type ['a]. *)
  type 'a t

  (** A value of type ['shape prod] is a vector whose shape is
      unknown. As soon as ['shape] is instantiated, at most one
      constructor matches this ['shape], which guarantees the validity
      to a vector of this ['shape]. *)
  type _ prod =
    | P1 : 'a t only -> 'a only prod
    | P2 : 'a t * 'b t -> ('a * 'b) prod
    | P3 : 'a t * 'b t * 'c t -> ('a * 'b * 'c) prod
    | P4 : 'a t * 'b t * 'c t * 'd t -> ('a * 'b * 'c * 'd) prod
    | P5 : 'a t * 'b t * 'c t * 'd t * 'e t -> ('a * 'b * 'c * 'd * 'e) prod

  type tfunction = { fapply : 'a. 'a t -> 'a t }
  val map : tfunction -> 'a prod -> 'a prod

  type action = { exec : 'a. 'a t -> unit }
  val iter : action -> 'a prod -> unit

  type action2 = { exec2 : 'a. 'a t -> 'a t -> unit }
  val iter2 : action2 -> 'a prod -> 'a prod -> unit

end

module MapProduct (T : sig type 'a t end)
  : MapProduct_sig with type 'a t = 'a T.t

(** [forever that] does [that] for ever if [that] calls [continues ()]
    when applied to [continues]. *)
val forever : ((unit -> 'a) -> 'a) -> 'a

(** [f $> g] does [f (); g ()] when applied to [()]. *)
val ( $> ) : (unit -> unit) -> (unit -> unit) -> unit -> unit

(** [!$ f] is [fun _ -> f ()]. *)
val ( !$ ) : (unit -> 'a) -> 'b -> 'a

(** [!* f] is [fun x -> f [x]]. *)
val ( !* ) : ('a list -> 'b) -> 'a -> 'b

(** [wait_for m p] blocks until [p] found what it wants in the
    mailbox [m]. *)
val wait_for : 'a Lwt_mvar.t -> ('a -> 'b option) -> 'b Lwt.t

(** [lwt_if c pt pe] lifts [if then else] to Lwt. *)
val lwt_if : bool Lwt.t -> 'a Lwt.t -> 'a Lwt.t -> 'a Lwt.t

(** [continue_while_is v ps] executes the sequence of [ps] while [v]
    is produced by each process. Returns the first result that does
    not produce [v] or [v] if there is no such result. *)
val continue_while_is : 'a -> (unit -> 'a Lwt.t) list -> 'a Lwt.t

val proj_1_3 : 'a * 'b * 'c -> 'a
val proj_2_3 : 'a * 'b * 'c -> 'b
val proj_3_3 : 'a * 'b * 'c -> 'c

}}

module ExtFilename : sig

  (** [temp_filename tmpdir prefix suffix] returns a fresh name for a
      file. Only guarantee that the file does not exist when called. *)
  val temp_filename : ?temp_dir:string -> string -> string -> string Lwt.t

end

(** Please remember that exceptions are Achilles'heel of ML. Indeed,
    there is no support in the language to statically track down
    where an exception can come from. Thus, exceptions are better
    used locally with clear barriers they cannot (or at least
    should not) escape.

    For this reason, we follow the convention that API-level functions
    of main subsystems shall not communicate their failure
    descriptions through exceptions but using a sum type.

    For general purpose utility functions upon which API-level
    functions are built, we allow exceptions by parameterizing them by
    the function to report exceptions.
*)

(** [('a, 'e) exn_free] represents a process producing ['a]s that does
    not raise exceptions but returns error code of type ['e]. *)
type ('a, 'e) exn_free =
  [`OK of 'a | `KO of 'e] Lwt.t

(** [('a, 'e) exn_abs] represents a function [f] which may raise
    error of type ['a] using a function [lraise] given as an
    argument. *)
type ('a, 'b, 'e) exn_abs =
    (('e -> 'b Lwt.t) -> 'a Lwt.t)

(** [ltry what] executes [what lraise] where [lraise] can be
    given any error of type ['a]. *)
val ltry : ('a, 'b, 'e) exn_abs -> ('a, 'e) exn_free

(** [lreturn x] is [fun _ -> return x]. *)
val lreturn : 'a -> ('a, 'b, 'e) exn_abs

(** [fail w] returns [`KO (`AssertFailure r)] when [r] is a failure
    produced by the assertion [w]. *)
val do_not_fail : (unit -> 'a)
  -> [
    `OK of unit
  | `KO of [> `AssertFailure of string ]
  ] Lwt.t

(** [!!> what raise] checks for the result [r] of [what] to
    [raise] an error if [r] matches [`KO e]. Otherwise if [r] matches
    [`OK x], returns [x]. *)
val ( !!> ) : (unit -> ('a, 'e) exn_free) -> ('a, 'b, 'e) exn_abs

(** [p1 >>> p2] is [fun l -> p1 l >> l2 l]. *)
val ( >>> ) : ('a -> 'b Lwt.t) -> ('a -> 'c Lwt.t) -> 'a -> 'c Lwt.t

(** [p1 >-> p2] is [fun l -> p1 l >>= fun x -> l2 x l]. *)
val ( >-> ) : ('a -> 'b Lwt.t) -> ('b -> 'a -> 'c Lwt.t) -> 'a -> 'c Lwt.t

(** [!>> p] is [p]. (Only meant for indentation purpose.) *)
val ( !>> )  : 'a -> 'a
val ( !>>> ) : 'a -> 'a
val ( !>>= ) : (unit -> 'a) -> 'a

(** [p1 >>>= p2] composes [p1] and [p2] in the error monad. *)
val ( >>>= ) :
  ('a, 'e) exn_free -> ('a -> ('b, 'e) exn_free) -> ('b, 'e) exn_free

(** [ f @* x ] is [fun () -> f x] *)
val ( @* ) : ('a -> 'b) -> 'a -> unit -> 'b

(** [ e @| p ] is [try_lwt let _ = e () in p with SmallJump ->
    p]. This combinator is useful to implement functions that wait for
    a function to raise an error. When error handling must be skipped
    for some reason, this argument can be filled with [small_jump]
    whose exception is immediately captured by the combinator. *)
exception SmallJump
val small_jump : 'a -> 'b Lwt.t
val ( @| ) : (unit -> 'a Lwt.t) -> (unit -> 'b Lwt.t) -> 'b Lwt.t

val warn_only : string -> 'a -> 'b Lwt.t

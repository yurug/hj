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

}}

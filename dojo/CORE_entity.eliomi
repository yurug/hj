(* -*- tuareg -*- *)

(** The reactive entities model. *)

(** Almost everything in the system is a reactive entity.

    A reactive entity of type ['a t] is characterized as follows:

   - An   entity  is   uniquely  identified   by  a   value   of  type
   {!CORE_identifier.t}.

   - An entity owns a {i content}  of type ['a]. Other entities may {i
   try} to update this content.  In that case, the concerned entity is
   asked to {i react} to this attempt.

   - An  entity   is  connected   to  a  set   of  entities,   its  {i
   dependencies}.  Each time one  of its dependencies is modified, the
   entity is asked  to {i react}. This reaction  may provoke an update
   that will be propagated to every  entity that depends on it. And so
   on, and  so forth. Of course,  as the graph of  dependencies is not
   necessarily acyclic, this process  can diverge.  For the moment, it
   is the  responsability of the programmer to  avoid non termination.
   In the future,  we will integrate a mechanism  to detect "too long"
   propagation of changes.

   - The state of every entity is replicated in a version file system
   by means of serialization to files. (See {!CORE_onthedisk_entity}
   for details.) The file system is managed using the VCS named
   Git. As a consequence the history of every entity is recorded.

   The following  module implements  the reactive model  for entities.
   It provides general functions to  interact with entities as well as
   a functor  to instantiate  a specific instance  of this  concept by
   fixing a specific type for content and a specific dynamic behavior.

*)

open CORE_inmemory_entity

(** The type of entity with content type ['a]. *)
type 'a entity
type 'a t = 'a entity
type some_t = SomeEntity : 'a t -> some_t

(** An entity may react to a change...*)
type 'a reaction =
    dependencies  (** ... of its dependencies *)
    -> 'a option  (** ... of its content *)
    -> 'a change

(** A [change] is a tranformation of the entity's content. *)
and 'a change = 'a -> 'a Lwt.t

(** On the client side, we can react to every change. *)
val channel: 'a t -> 'a CORE_client_reaction.c

(** The following module signature specifies the general operations
    over entities. *)
module type S = sig

  (** The operations are indexed by the type of the content. *)
  type data
  type t = data entity
  type reference deriving (Json)

  (** [make ?init reaction id] returns a representation of the entity
      [id] in memory whose behavior is defined by [reaction].

      If [id] does not exist, then [init] is used as an initial
      representation. If [init] is not provided, then the error
      [`UndefinedEntity id] is returned.

      If [init] is provided and [id] already exists, the exception
      [`AlreadyExists id] is returned.

      Notice that having requested for a representation of an entity in
      memory does not imply that its content is up to date. We prefer a
      lazy approach: only [read] and [change] will trigger the
      computation of the actual content as a reaction to the current
      state of the system.
  *)
  val make:
    ?init:(data * dependencies * CORE_property.set)
    -> ?reaction:data reaction
    -> CORE_identifier.t ->
    [ `OK of t
    | `KO of [>
      | `UndefinedEntity of CORE_identifier.t
      | `AlreadyExists   of CORE_identifier.path
      | `SystemError     of string
    ]] Lwt.t

  (** [identifier e] returns the identifier of [e]. *)
  val identifier : t -> CORE_identifier.t

  (** [properties e] returns the properties of [e]. *)
  val properties : t -> CORE_property.set

  (** [change e c] asks for the replacement of [e]'s content by [c
      e].  This attempt triggers the reaction of the entity, which is
      not atomic. This operation may block if a reaction is already
      running.

      During this reaction, other attempts  to update the content of [e]
      are suspended.

      The  reaction also  propagates  the change  to  every entity  that
      refers to [e]. These entities are updated asynchronously.

      The code describing the change is atomic with respect
      to the state of the entity.
  *)
  val change : t -> data change -> unit Lwt.t

  (** [observe e o] evaluates [o] with the up-to-date content of [e].
      As long as [o] is not finished, the requested changes to [e] are
      suspended.
  *)
  val observe : t -> (data -> 'a Lwt.t) -> 'a Lwt.t

  (** [refer_to x] creates a reference to [t]. *)
  val refer_to : t -> reference

  (** [deref x] follows the reference [x]. *)
  val deref : reference ->
    [ `OK of t
    | `KO of [>
      | `UndefinedEntity of CORE_identifier.t
      | `SystemError     of string
    ]] Lwt.t

end

(** The following module interface has to be implemented to instantiate
    a type of entity on a specific content type. *)
module type U = sig

  (** The content type. It must be deriving Json. *)
  type data deriving (Json)

  (** The reactive behavior of the entity. *)
  val react : data reaction

end

(** [passive] is a reaction that does nothing. *)
val passive : 'a reaction

(** Instantiate a set of operations over a specific type of entity. *)
module Make (I : U) : S with type data = I.data

(** Unit testing. *)
module Tests : sig
  val check : (string -> unit) -> [
    `OK of unit
  | `KO of [> CORE_errors.all ]
  ] Lwt.t
end

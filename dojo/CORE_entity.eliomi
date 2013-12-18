(* -*- tuareg -*- *)

(** The reactive entities model. *)

(** Almost everything in the system is a reactive entity.

    A reactive entity of type [('a, 'c) t] is characterized as follows:

   - An   entity  is   uniquely  identified   by  a   value   of  type
    {!CORE_identifier.t}.

    - An entity has a {i state} of type ['a
    CORE_inmemory_entity.meta]. Other entities may {i try} to update
    this state using high-level changes of type ['c].  In that case,
    the concerned entity is asked to {i react} to this attempt. It
    validates or invalidates this update by turning it into low-level
    changes and may also trigger more changes.

    - An  entity   is  connected   to  a  set   of  entities,   its  {i
    dependencies}.  Each time one  of its dependencies is modified, the
    entity is asked  to {i react}. This reaction  may provoke an update
    that will be propagated to every  entity that depends on it. And so
    on, and  so forth. Of course,  as the graph of  dependencies is not
    necessarily acyclic, this process  can diverge.  For the moment, it
    is the  responsability of the programmer to  avoid non termination.

    - The state of every entity is replicated in a versioned file system
    by means of serialization to files. (See {!CORE_onthedisk_entity}
    for details.) The file system is managed using the VCS named
    Git. As a consequence the history of every entity is recorded.

    The following module implements the reactive model for entities.
    It provides general functions to interact with entities as well as
    a functor to instantiate a specific instance of this concept by
    fixing a specific type for content, a specific type for high-level
    changes and a specific dynamic behavior.

*)

(** The following module defines entities' state whose content has
    type ['a] and low-level ['a change] on this state. *)
open CORE_inmemory_entity
open CORE_identifier

(** The type of entity with content of type ['a] and high-level
    changes of type ['c]. *)
type ('a, 'c) entity
type ('a, 'c) t = ('a, 'c) entity

(** An entity ... *)
type ('a, 'c) reaction =
    'a meta                  (** with ['a meta] state may react to *)
    -> dependencies          (** a change of one of its dependencies ... *)
    -> 'c list               (** or to external requests to change ... *)
    -> ('c -> unit Lwt.t)    (** by scheduling a change or ... *)
    -> 'a state_change Lwt.t (** by requesting an immediate internal change. *)

{shared{

(** A timestamp represents a version number for an entity. *)
type timestamp deriving (Json)

(** Two kinds of events are possibly happening to an entity. *)
type event =

  (** A dependency of the entity has been updated, so at some point,
      the entity will be asked to update itself *if necessary*. A
      client may be interested in triggering this update. In that
      case, it simply has to use an observer process on that
      entity. This observation will force the update to happen
      immediately.

      In other words, we make a distinction between on the one hand
      "potential observers", that are interested in being notified
      when an event happens and "active observers", that are
      interested in forcing events to happen to have an up-to-date
      observation of the entity.
  *)
  | MayChange

  (** The entity has been updated. *)
  | HasChanged

}}

(** On the client side, we can react to every change. *)
val channel: ('a, 'c) t -> event CORE_client_reaction.c

(** The following module signature specifies the general operations
    over entities of content type [data] and high-level changes of
    type [change]. *)
module type S = sig

  (** The type of the content. *)
  type data

  (** The type of high-level changes that are available to external
      entities. *)
  type change

  (** The specific instance of the entity concept for this type of
      data and this type of external change. *)
  type t = (data, change) entity

  (** [make ?init reaction id] returns a representation of the entity
      [id] in memory whose behavior is defined by [reaction].

      If [id] does not exist, then [init] is used as an initial
      representation. If [init] is not provided, then the error
      [`UndefinedEntity id] is returned.

      If [init] is provided and [id] already exists, the exception
      [`AlreadyExists id] is returned.

      Notice that having requested for a representation of an entity in
      memory does not imply that its content is up-to-date. We prefer a
      lazy approach: only [read] and [change] will trigger the
      computation of the actual content as a reaction to the current
      state of the system.
  *)
  val make:
    ?init:(data * dependencies * CORE_property.set * CORE_source.filename list)
    -> ?reaction:(data, change) reaction
    -> CORE_identifier.t ->
    [ `OK of t
    | `KO of [>
      | `UndefinedEntity of CORE_identifier.t
      | `AlreadyExists   of CORE_identifier.path
      | `SystemError     of string
    ]] Lwt.t

  (** [change e c] externally asks to a change [c] to happen
      asynchronously. At some point, this request will trigger the
      reaction of the entity, which is not atomic.

      The reaction also propagates the change to every entity that
      refers to [e]. These entities will react asynchronously.

      If [immediate] is set, the reaction without further delay.
      Otherwise, we are lazy: the change is not applied unless a
      process actively observes the state of entity. *)
  val change
    : ?immediate:bool -> ?who:identifier -> t -> change -> unit Lwt.t

  (** [observe e o] evaluates [o] with the content of [e].

      if [fresh = true] then pending changes are applied before
      observation so that the content is as fresh as possible.

      As long as [o] is not finished, the requested changes to [e] are
      suspended. *)
  val observe
    : ?fresh:bool -> ?who:identifier -> t -> (data meta -> 'a Lwt.t) -> 'a Lwt.t

  (** [identifier e] returns the identifier of [e]. This information
      will never change during the life of [e]. *)
  val identifier : t -> CORE_identifier.t

  (** [log k e] returns (at most) the [k] last changes previously
      applied to [e] with their timestamp. The most recent comes
      first. *)
  val log : t -> int -> (change * timestamp) list

end

(** The following module interface has to be implemented to instantiate
    a type of entity on a specific content type. *)
module type U = sig

  (** The content type. It must be deriving Json. *)
  type data deriving (Json)

  (** Type high-level change type. *)
  type change

  (** The reactive behavior of the entity. One may assume that
      the state of the entity will not change during the
      execution of [react]. *)
  val react : (data, change) reaction

  (** For the logging facilities, we expect a [change] to
      be showable in a (one line long) string. *)
  val string_of_change : change -> string

end

(** Instantiate a set of operations over a specific type of entity. *)
module Make (I : U)
: S with type data = I.data and type change = I.change

(** Data-only entity. *)
module type D = sig
  type data deriving (Json)
  val string_of_replacement : data -> string
end

(** Instantiate a set of operations over a specific type of entity
    that only consists of data. *)
module Passive (I : D)
: U
  with type data = I.data
  and type change = I.data CORE_inmemory_entity.state_change

(** Unit testing. *)
module Tests : sig
  val check : (string -> unit) -> [
    `OK of unit
  | `KO of [> CORE_errors.all ]
  ] Lwt.t
end

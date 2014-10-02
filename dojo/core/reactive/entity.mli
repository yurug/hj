(* -*- tuareg -*- *)

(** The reactive entities model. *)

(** Almost everything in the system logic is a reactive entity.

    A reactive entity of type [('a, 'c) t] is characterized as follows:

    - An entity is uniquely identified by a value of type
    {!Identifier.t}.

    - An entity has a {i state} of type ['a InMemory.meta].

    Other entities may {i try} to update this state using high-level
    changes of type ['c].  In that case, the concerned entity is asked
    to {i react} to this attempt. It validates or invalidates this
    update by turning it into low-level changes.

    - An entity is connected to a set of entities, called its {i
    dependencies}.

    Each time one of its dependencies is modified, the entity is asked
    to {i react}. This reaction may provoke an update that will be
    propagated to every entity that depends on it. And so on, and so
    forth. Of course, as the graph of dependencies is not necessarily
    acyclic, this process can diverge.  For the moment, it is the
    responsability of the programmer to avoid non termination.

    - The internal state of every entity is replicated in a versioned
    file system by means of serialization to files. (See
    {!OnDisk} for details.) The file system is managed
    using the VCS named Git. As a consequence the history of every
    entity is recorded.

    - The external state of the entity is represented using the
    {!Facts}.

    - All the events (this includes interactions between entities
    and interactions between users and entities) are recorded as events
    in the {!Log} system.

    The following module implements the reactive model for entities.
    It provides general functions to interact with entities as well as
    a functor to instantiate a specific instance of this concept by
    fixing a specific type for content, a specific type for high-level
    changes and a specific dynamic behavior (i.e. reaction function).

*)

(** The following module defines entities' state whose state has type
    ['a] and such that high-level ['a state_change]s are admitted on
    this state. *)
open InMemory
open Identifier

(** The type of entity with content of type ['a] and high-level
    changes of type ['c]. *)
type ('a, 'c) entity
type ('a, 'c) t = ('a, 'c) entity

type ('a, 'c) reaction =
    (** An entity ... *)
    'a meta               (** ... with ['a meta] state may react to *)
    -> dependencies          (** ... a change of one of its dependencies *)
    -> 'c list               (** ... or to external requests to change *)
    -> ('c -> unit Lwt.t)    (** ... by scheduling a change or *)
    -> 'a state_change Lwt.t (** ... by asking for immediate internal update. *)

(** Two kinds of events are possibly observable about an entity: *)
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
      observation of the entity. *)
  | MayChange

  (** The entity has been updated. *)
  | HasChanged

(** Some data structures contain entities with heterogeneous
    content types. In that case, this type is hidden behind
    an existential quantification. *)
type some_t = SomeEntity : ('a, 'c) t -> some_t

(** On the client side, we can react to every change. *)
val channel: ('a, 'c) t -> event Lwt_stream.t

(** The unique identifier of an entity. *)
val identifier: ('a, 'c) t -> Identifier.t

(** [exists id] returns [true] iff [id] exists. *)
val exists : Identifier.t -> bool

(** The following module signature specifies the general operations
    over entities of content type [data] and high-level changes of
    type [change]. *)
module type S = sig

  type identifier = Identifier.t deriving (Json)

  (** The type of the content. *)
  type data

  (** The type of high-level changes that are available to external
      entities. *)
  type change

  (** The specific instance of the entity concept for this type of
      data and this type of external change. *)
  type t = (data, change) entity

  (** A string representation of this kind of entity. *)
  val kind : string

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
      state of the system. This means that if some effects are expected
      to be produced by an entity reaction, it might happen a long time
      after the creation of this entity in memory.
  *)
  val make:
    ?init:(data * dependencies * Resource.t list)
    -> ?reaction:(data, change) reaction
    -> Identifier.t ->
    [ `OK of t
    | `KO of [>
      | `UndefinedEntity of Identifier.t
      | `AlreadyExists   of Identifier.path
      | `SystemError     of string
      | `InternalError   of exn
    ]] Lwt.t

  (** [shutdown ()] deactivates all the entities of this kind. *)
  val shutdown : unit -> unit

  (** [change e c] externally asks for a change [c] to happen
      asynchronously. At some point, this request will trigger the
      reaction of the entity, which is not atomic.

      The reaction also propagates the change to every entity that
      refers to [e]. These entities will react asynchronously.

      If [immediate] is set, the reaction will happen without further
      delay.  Otherwise, we are lazy: the change is not applied unless
      a process actively observes the state of the entity. *)
  val change : ?who:identifier -> t -> change -> unit Lwt.t

  (** [observe e o] evaluates [o] with the content of [e].

      As long as [o] is not finished, the requested changes to [e] are
      suspended. *)
  val observe : ?who:identifier -> t -> (data meta -> 'a Lwt.t) -> 'a Lwt.t

  (** [identifier e] returns the identifier of [e]. This information
      will never change during the life of [e]. *)
  val identifier : t -> Identifier.t

  (** [resources e] returns all the resource of [e]. *)
  val resources : t -> Resource.name list

  (** [resource_versions e name] returns all the versions of resource
      [name] in [e]. *)
  val resource_versions : t -> Resource.name -> [
    `KO of [> `SystemError of string ]
  | `OK of VFS.version list
  ] Lwt.t

  (** [resource e name] returns the resource of [e] named [name]. *)
  val resource : t -> ?version:string -> Resource.name -> [
    `KO of [> `SystemError of string | `NoSuchVersion ]
  | `OK of Resource.t * Identifier.path
  ] Lwt.t

  (** [import_resource e r on_finished] inserts [r] in the resources of [e].
      When import is done, [on_finished ()] is fired. *)
  val import_resource : t -> Resource.t -> (unit -> unit Lwt.t) -> [
    `KO of [> `SystemError of string ]
  | `OK of bool
  ] Lwt.t

  (** [publish e flag r] makes the resource [r] of [e] public if
      [flag = true], private otherwise. *)
  val publish : t -> bool -> Resource.name -> [
    `KO of [> `SystemError of string ]
  | `OK of unit
  ] Lwt.t

  (** [is_public_resource e r] returns [true] if [r] is published by [e]. *)
  val is_public_resource : t -> Resource.name -> bool
end

(** The following module interface has to be implemented to instantiate
    a type of entity on a specific content type. *)
module type U = sig

  (** The content type. It must be deriving Json. *)
  type data deriving (Json)

  (** Type high-level change type. *)
  type change

  (** A string representation for this kind of entity. *)
  val kind : string

  (** The reactive behavior of the entity. One can assume that
      the state of the entity will not change during the
      execution of [react]. *)
  val react : (data, change) reaction

  (** For the logging facilities, we expect a [change] to
      be showable in a (one line long) string. *)
  val string_of_change : change -> string

  (** For future backward compatibility, we require a converter from old
      versions of the entity state to the current one. *)
  val current_version : string
  val converters : (module OnDisk.Converter with type destination = data) list

end

(** Instantiate a set of operations over a specific type of entity. *)
module Make (I : U)
: S with type data = I.data and type change = I.change

(** Data-only entity. *)
module type D = sig
  type data deriving (Json)
  val kind : string
  val string_of_replacement : data -> string
  val current_version : string
  val converters : (module OnDisk.Converter with type destination = data) list
end

(** Instantiate a set of operations over a specific type of entity
    that only consists of data. *)
module Passive (I : D)
: U
  with type data = I.data
  and type change = I.data InMemory.state_change

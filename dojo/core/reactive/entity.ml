(* -*- tuareg -*- *)

(** The reactive entities model. *)

open Printf
open Lwt

open Identifier
open InMemory
open Error
open ExtPervasives

(* ****************** *)
(*  Type definitions  *)
(* ****************** *)

(** Two kinds of events are possibly happening to an entity. *)
type event =
  (** A dependency of the entity has been updated, so at
      some point, the entity will be asked to update itself
      if necessary. *)
  | MayChange

   (** The entity has been updated. *)
  | HasChanged

type ('a, 'c) reaction =
    (** An entity ... *)
    'a meta               (** ... with ['a meta] state may react to *)
    -> dependencies          (** ... a change of one of its dependencies *)
    -> 'c list               (** ... or to external requests to change *)
    -> ('c -> unit Lwt.t)    (** ... by scheduling a change or *)
    -> 'a state_change Lwt.t (** ... by asking for immediate internal update. *)

(** The state of an entity can be up-to-date or modified. In that
    later case, the modified dependencies and the scheduled changes
    are attached to the entity. *)
and 'c state =
  | UpToDate
  | Modified of dependencies * 'c Queue.t

(** Here is the internal representation of the entity:

    - the [description] field stores the in-memory
    representation of the entity. This description is
    replicated in the versioned file system.

    - the [state] denotes the status of the entity with
    respect to its dependencies and scheduled changes.

    - the [reaction] implements the behavior of the entity.

    - the [channel] and [push] fields give a communication
    device from the entity on the server to the client-side
    code.
*)
and ('a, 'c) entity = {
  mutable description : 'a meta;
  mutable state       : 'c state;
  (*   *) reaction    : ('a, 'c) reaction;
  (*   *) channel     : event Lwt_stream.t;
  (*   *) push        : event -> unit;
  (*   *) react_cond  : unit Lwt_condition.t;
  (*   *) commit_lock : Lwt_mutex.t;
  (*   *) commit_cond : unit Lwt_condition.t;
  mutable mode        : [ `Commit | `Observe of int ];
  mutable log         : ('c * Timestamp.t) list;
  mutable last_save   : Timestamp.t
}

(** Shortcut for the type of entities. *)
and ('a, 'c) t = ('a, 'c) entity

(** Some data structures contain entities with heterogeneous
    content types. In that case, this type is hidden behind
    an existential quantification. *)
type some_t = SomeEntity : ('a, 'c) t -> some_t

(** Accessor to the unique identifier of the entity. *)
let identifier e = InMemory.identifier e.description

(** Check the existence of an entity. *)
let exists id = OnDisk.exists id

(* ********************** *)
(*  Reverse dependencies  *)
(* ********************** *)

(** The server maintains an association between each entity
    and its reverse dependencies. *)
module EntitySet = Hashtbl.Make (struct
  type t = some_t

  let hash (SomeEntity x) =
    Hashtbl.hash (identifier x)

  let equal (SomeEntity x) (SomeEntity y) =
    Identifier.compare (identifier x) (identifier y) = 0
end)

module IdHashtbl = struct
  include Hashtbl.Make (Identifier)
  let get h k =
    try Some (find h k) with Not_found -> None
end

module Watchers = IdHashtbl

type watchers = (dependency_kind * identifier list) EntitySet.t Watchers.t

let watchers = Watchers.create 13

(** [watchers_of id] returns the set of all reverse
    dependencies of [id]. *)
let watchers_of id =
  try
    Watchers.find watchers id
  with Not_found ->
    let s = EntitySet.create 13 in
    Watchers.add watchers id s;
    s

(** [e] depends on [e']. *)
let register_dependency e (id, rel) =
  EntitySet.replace (watchers_of id) (SomeEntity e) rel

(** [propagate_change id] wakes up all the reverse dependencies of [e]. *)
let propagate_change id =
  (** We traverse the reverse dependencies of [e] and trigger a
      reaction to the change of [id] by marking them as being
      modified. *)
  let wake_up (SomeEntity e) (l, xs) =
    let (dependencies, queue) =
      match e.state with
        | UpToDate ->
          (empty_dependencies, Queue.create ())
        | Modified (dependencies, queue) ->
          (dependencies, queue)
    in

    (** We also notify [e] that [id] has one of its dependencies that
        has changed. *)
    e.state <- Modified (push dependencies (id, (l, xs)), queue);
    Lwt_condition.signal e.react_cond ();
  in
  EntitySet.iter wake_up (watchers_of id)

(** [channel e] gives a device for client-side code to be notified
    each time [e] is changed. *)
let channel e = e.channel

(* *********************** *)
(*  Instantiation functor  *)
(* *********************** *)

(** The base interface of an entity module. *)
module type S = sig
  type identifier = Identifier.t deriving (Json)
  type data
  type change
  type t = (data, change) entity

  val kind : string

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

  val shutdown
    : unit -> unit

  val change
    : ?who:identifier -> t -> change -> unit Lwt.t

  val observe
    : ?who:identifier -> t -> (data meta -> 'a Lwt.t) -> 'a Lwt.t

  val identifier : t -> Identifier.t

  val resources : t -> Resource.name list

  val resource_versions : t -> Resource.name -> [
    `KO of [> `SystemError of string ]
  | `OK of VFS.version list
  ] Lwt.t

  val resource : t -> ?version:string -> Resource.name -> [
    `KO of [> `SystemError of string | `NoSuchVersion ]
  | `OK of Resource.t * Identifier.path
  ] Lwt.t

  val import_resource : t -> Resource.t
    -> (unit -> unit Lwt.t) -> [
    `KO of [> `SystemError of string ]
  | `OK of bool
  ] Lwt.t

  val publish : t -> bool -> Resource.name -> [
    `KO of [> `SystemError of string ]
  | `OK of unit
  ] Lwt.t

  val is_public_resource : t -> Resource.name -> bool

end

(** The client must provide the following information
    about his specific entity. *)
module type U = sig
  type data deriving (Json)
  type change
  val kind : string
  val react : (data, change) reaction
  val string_of_change : change -> string
  val current_version : string
  val converters : (module OnDisk.Converter with type destination = data) list
end

module type D =
sig
  type data deriving (Json)
  val kind : string
  val string_of_replacement : data -> string
  val current_version : string
  val converters : (module OnDisk.Converter with type destination = data) list
end

module Passive (I : D) : U
  with type data = I.data
  and type change = I.data InMemory.state_change
= struct
  type data = I.data deriving (Json)
  type change = I.data InMemory.state_change

  let kind = I.kind

  let react _ _ cs _ =
    return (InMemory.state_changes cs)

  let string_of_change =
    InMemory.string_of_state_change I.string_of_replacement

  let current_version = I.current_version
  let converters = I.converters
end

(** The implementation of the operations over entities. *)
module Make (I : U) : S
with type data = I.data
and type change = I.change
= struct

  type identifier = Identifier.t deriving (Json)

  type data = I.data

  type change = I.change

  let kind = I.kind

  type t = (data, change) entity

  (** The following module implements on-the-disk replication
      of entity descriptors. *)
  module OTD = OnDisk.Make (I)

  (* ****** *)
  (*  Pool  *)
  (* ****** *)

  (** There must be at most one instance of an entity.  We use a pool
      to maintain the set of alive entities. *)
  let (loaded, load, iter_on_pool, empty_pool) =
    let pool = IdHashtbl.create 13 in
    let iter f =
      let l = ref [] in
      IdHashtbl.iter (fun k v -> l := (k, v) :: !l) pool;
      Lwt_list.iter_s (fun (x, v) -> f x v) !l
    in
    let clear () = IdHashtbl.clear pool in
    (IdHashtbl.get pool, IdHashtbl.add pool, iter, clear)

  (* FIXME: A quick a dirty code. We should wait for all the processes
     FIXME: to be finished for the saving process to be completed for
     FIXME: sure.  *)

  let active = ref true

  let rec save_pool () =
    iter_on_pool (fun id e ->
      save_on_disk ~now:true e
      >>= fun _ -> return ()
    ) >> (
      return (Log.debug (identifier_of_string I.kind) "Saving pool done.")
    )

  and shutdown () =
    (* FIXME: We should properly stop, that is:
       1. Reject all new "external changes"
       2. Wait for all the "internal changes" to have converged.

       The current implementation is still safe because the
       implementation of the code of each entity does not assume that
       a change it has requested is applied as long as it has not been
       checked the result by itself. In other words, between two updates
       the internal state of an entity is always consistent because its
       consistency does not depend on the other entities' states. *)
    active := false;
    Lwt.async (fun () ->
      save_pool () >> return (empty_pool ())
    )

  (** [awake id reaction] loads the entity named [id] from the
      file system and instantiate it in memory. *)
  and wakeup id reaction =
    OTD.load id >>>= fun description ->
    (** Notice that the following sequence of operations are
        atomic w.r.t. the concurrency model. *)
    let channel, push = Lwt_stream.create () in
    let push x = push (Some x) in
    let react_cond = Lwt_condition.create () in
    let commit_lock = Lwt_mutex.create () in
    let commit_cond = Lwt_condition.create () in
    let e = {
      description; reaction; state = UpToDate;
      channel; push; react_cond;
      commit_lock; commit_cond;
      mode = `Observe 0;
      log = [];
      last_save = Timestamp.current ()
    } in
    load id e;
    List.iter (register_dependency e)
      (dependency_image (dependencies e.description)
      );
    Lwt.async (fun () ->
      let rec tick () =
        (** This is the only place where update is called. *)
        update e
        (* FIXME: I would like to use the following condition instead of
           FIXME: this ugly active loop. Yet, for some reason, it does
           FIXME: work. For the moment, I stay with an active loop
           FIXME: because it is not expansive but this is clearly
           FIXME: not satisfactory... *)
        (*        >> Lwt_condition.wait e.react_cond *)
        >> Lwt_unix.yield ()
        >> Lwt_unix.sleep 0.1
        >>= tick
      in
      tick ()
    );
    return (`OK e)

  (** [initialize init deps fnames id] creates the on-the-disk
      representation of [id] so that it can be loaded afterwards.
      Precondition: [id] must not already exist. *)
  and initialize init deps fnames id =
    if OnDisk.exists id then
      return (`KO (`AlreadyExists (path_of_identifier id)))
    else
      OTD.save (InMemory.make id deps fnames init)

  (* ************************** *)
  (*  Operations over entities  *)
  (* ************************** *)

  (** [make init reaction id] deals with the instanciation of [id] ... *)
  and make ?init ?(reaction = I.react) id =
    match init with
      | Some (init, dependencies, filenames) ->
        (** This is the first time for [id], we make room for it in
            the file system ... *)
        initialize init dependencies filenames id
        (** ... and we instanciate it from that. *)
        >>>= fun () ->
        make ~reaction id

      | None ->
        (** [id] already exists somewhere ...*)
        match loaded id with
          (** ... in memory, we return this unique instance. *)
          | Some e -> return (`OK e)
          (** ... on the disk, we load it from the file system. *)
          | None -> wakeup id reaction

  and wait_to_be_observer_free e commit =
    Lwt_mutex.with_lock e.commit_lock (fun () ->
      (** Set the commit mode, no observers are allowed to run. *)
      e.mode <- `Commit;
      (** Nobody is watching! Do your stuff! *)
      commit () >>= fun () ->
      return (
        e.mode <- `Observe 0;
        Lwt_condition.signal e.commit_cond ()
      )
    )

  (** [apply deps e c] applies the effective changes [cs] to the
      content of [e]. *)
  and apply dependencies e cs =

    let state = e.description in

    (** Notice that we globally maintain the invariant that two
        reactions cannot run concurrently on the same entity. So,
        during the execution of a reaction the current content of the
        entity cannot change.

        Furthermore, even if the actual modification of the entity
        state is done when no observer is watching, the reaction
        itself can be computed concurrently to observations: as soon
        as nothing is observable from the point of view of these
        observations, this is fine. *)
    let change_later c = change e c in
    let card_changes =
      List.length cs + List.length (to_list dependencies)
    in
    if card_changes = 0 then
      return ()
    else
      e.reaction state dependencies cs change_later >>= function
        | NoUpdate ->
          return ()

        | llc ->
          wait_to_be_observer_free e (fun () ->
            let old = e.description in
            e.description <- InMemory.update e.description llc;
            List.iter (fun c -> Log.debug (identifier e) (I.string_of_change c)) cs;
            (* FIXME: The following optimization is unsafe: we must ensure *)
            (* FIXME: that every entity is finally saved. *)
            save_on_disk e >>= function
              | `KO error ->
                (* FIXME: Warn in the log. *)
                return (e.description <- old)
              | `OK _ ->
                e.push HasChanged;
                return ()
          ) >>= fun () -> return (propagate_change (identifier e))

  and save_on_disk ?(now=false) e =
    (* 60. must be a parameter. *)
    if (Timestamp.compare (timestamp e.description) e.last_save <> 0
      && (now || Timestamp.older_than 60. e.last_save))
    then begin
      e.last_save <- timestamp e.description;
      OTD.save e.description
    end
    else return (`OK ())

  (** [update e] is the process that applies scheduled changes. *)
  and update e =
    match e.state with
      | UpToDate ->
        return ()

      | Modified (dependencies, queue) ->
        let cs = Queue.fold (fun cs c -> c :: cs) [] queue in
        e.state <- UpToDate;
        apply dependencies e (List.rev cs)

  (** As long as a change is being applied, we have to
      push other required changes into a queue. *)
  and now_only_accumulate_changes e =
    e.state <- Modified (empty_dependencies, Queue.create ())

  and change ?who e c =

    match e.state with
      | UpToDate ->
        (** Good, the change is applied immediately. *)
        now_only_accumulate_changes e;
        change ?who e c

      | Modified (dependencies, queue) ->
        (** This change is scheduled for further application. *)
        Queue.push c queue;
        return (Lwt_condition.signal e.react_cond ())

  let bad_assumption_descriptor =
    Log.make_event_descriptor "Entity.bad_assumption" Facts.string

  let bad_assumption e msg =
    Log.log (identifier e) bad_assumption_descriptor msg

  let observe (type a) ?who (e : t) (o : data meta -> a Lwt.t) : a Lwt.t =
    let master = ref false in
    let rec aux () =
      let incr_observers () =
        match e.mode with
          | `Commit ->
            bad_assumption e "Observing mode is assumed at this point.";
            assert false

          | `Observe x ->
            e.mode <- `Observe (x + 1)
      in
      if Lwt_mutex.is_locked e.commit_lock
        || not (Lwt_mutex.is_empty e.commit_lock)
      then
        match e.mode with
          | `Commit ->
            (** A commit is being applied, wait for it to finish. *)
            Lwt_condition.wait e.commit_cond
            (** and try to observe again. *)
            >>= aux

            | `Observe x -> return (
              (** An observer already took the mutex. *)
              (** Let us register our presence to him. *)
              incr_observers ()
            )
        else (
            (** The master observer owns the lock and it will release
                it only when no more observation is required. This
                model clearly gives priority to observations over
                changes, which is a good property for user-reactivity.
                Yet, we will have to be careful not to totally starve
                changes processes...

                If I am correct, the fact that an observation first
                calls [update] for changes to be applied prevents an
                unbound creation of observers that could blocked the
                effective application of changes.
            *)
          Lwt_mutex.lock e.commit_lock >>= fun () -> return (

            (** Neither a change nor an observer is working on that entity. *)
            assert (e.mode <> `Commit);

            (** Entity, you are now observed: you cannot change while I am
                watching you! *)
            incr_observers ();

            (** This observer is called the "master" observer. *)
            master := true;
          )
        )
      in
      aux ()
      >>= fun () -> (
         (** At this point, the lock is taken and the mode is to observe. *)
         assert (Lwt_mutex.is_locked e.commit_lock);
         assert (match e.mode with `Observe _ -> true | _ -> false);

         (** A good place for the observation to take place. *)
         lwt ret = try_lwt
                     o e.description
           with exn ->
             bad_assumption e (
               Printf.sprintf
                 "Observers do not raise exceptions (Here: %s)."
                 (Printexc.to_string exn)
             );
             assert false
         in

         (** We are done with this observation. Let us release
             some time for other processes... *)
         (if !master then

             (** The master waits for other observers to finish their
                 observations. *)
             let rec wait_for_other_observers () =
               match e.mode with
                 | `Observe 1 -> return (
                   e.mode <- `Observe 0;
                   Lwt_mutex.unlock e.commit_lock
                 )
                 | `Observe x when x > 1 ->
                   Lwt_condition.wait e.commit_cond
                   >>= wait_for_other_observers
                 | `Observe x -> assert false
                 | `Commit -> assert false
             in
             wait_for_other_observers () >>= fun () -> return (
             )

          else (
            (** For others, it simply means to quit the master. *)
            match e.mode with
              | `Observe x -> return (
                if not (x >= 1) then
                  bad_assumption e
                    (sprintf "Still (%d >= 1) when slave observer stops." x);
                e.mode <- `Observe (x - 1);
                Lwt_condition.signal e.commit_cond ()
              )
              | `Commit ->
                return ()
          )
         )
         >>= fun () -> (
           return ret
         )
      )

  let identifier = identifier

  let resource e ?version x =
    OnDisk.load_resource (identifier e) ?version x

  let import_resource e s on_finished =
    e.description <- InMemory.(update e.description (UpdateResources [s]));
    OnDisk.save_resource (identifier e) s on_finished

  let publish e f r =
    e.description <- InMemory.(
      update e.description (UpdateResourceStatus (r, f))
    );
    return (`OK ())

  let is_public_resource e r =
    InMemory.is_public_resource e.description r

  let resources e = InMemory.resources e.description

  let resource_versions e name =
    OnDisk.resource_versions (identifier e) name

  let _ =
    Lwt.async (fun () ->
      let rec forever () =
        return (save_pool ())
        >> Lwt_unix.sleep 900.
        >> if !active then forever () else return ()
      in
      forever ()
    )

end

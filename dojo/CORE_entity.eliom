(* -*- tuareg -*- *)

(** The reactive entities model. *)

open Lwt

open CORE_identifier
open CORE_standard_identifiers
open CORE_inmemory_entity
open COMMON_pervasives

(* ****************** *)
(*  Type definitions  *)
(* ****************** *)

{shared{

(** A timestamp represents a version number for an entity. *)
type timestamp = float deriving (Json)

(** Two kinds of events are possibly happening to an entity. *)
type event =
    (** A dependency of the entity has been updated, so at
        some point, the entity will be asked to update itself
        if necessary. *)
  | MayChange

   (** The entity has been updated. *)
  | HasChanged

}}

(** An entity ... *)
type ('a, 'c) reaction =
    CORE_identifier.t      (** [x] may react to ... *)
    -> dependencies        (** a change of one of its dependencies ... *)
    -> 'c list             (** or to external requests to change ... *)
    -> ('c -> unit)        (** by scheduling an asynchronous change.*)
    -> 'a change           (** or applying an immediate internal change. *)

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
  (*   *) channel     : event CORE_client_reaction.c;
  (*   *) push        : event -> unit;
  (*   *) commit_lock : Lwt_mutex.t;
  (*   *) commit_cond : unit Lwt_condition.t;
  mutable mode        : [ `Commit | `Observe of int ];
  (*   *) log         : ('c * timestamp) Queue.t;
}

(** Shortcut for the type of entities. *)
and ('a, 'c) t = ('a, 'c) entity

(** Some data structures contain entities with heterogeneous
    content types. In that case, this type is hidden behind
    an existential quantification. *)
type some_t = SomeEntity : ('a, 'c) t -> some_t

(** Accessor to the dependencies. *)
let dependencies e = CORE_inmemory_entity.dependencies e.description

(** Accessor to the unique identifier of the entity. *)
let identifier e = CORE_inmemory_entity.identifier e.description

(** Accessor to the properties. *)
let properties e = CORE_inmemory_entity.properties e.description

(** Accessor to the timestamp. *)
let timestamp e = CORE_inmemory_entity.timestamp e.description

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
    CORE_identifier.compare (identifier x) (identifier y) = 0
end)

module IdHashtbl = struct
  include Hashtbl.Make (CORE_identifier)
  let get h k = try Some (find h k) with Not_found -> None
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
    e.push MayChange;
    let (dependencies, queue) =
      match e.state with
        | UpToDate ->
          (empty_dependencies, Queue.create ())
        | Modified (dependencies, queue) ->
          (dependencies, queue)
    in

    (** We also notify [e] that [id] has one of its dependencies that
        has changed. *)
    e.state <- Modified (push dependencies (id, (l, xs)), queue)
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
  type data
  type change
  type t = (data, change) entity

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

  val change : ?immediate:bool -> t -> change -> unit Lwt.t
  val observe : t -> (data meta -> 'a Lwt.t) -> 'a Lwt.t
  val identifier : t -> CORE_identifier.t
end

(** The client must provide the following information
    about his specific entity. *)
module type U = sig
  type data deriving (Json)
  type change
  val react : (data, change) reaction
  val string_of_change : change -> string
end

module MakePassive (I : sig
  type data deriving (Json)
  val string_of_replacement : data -> data -> string
end) : U = struct
  type data = data deriving (Json)
  type change = data CORE_inmemory_entity.change

  let react _ _ cs _ =
    CORE_inmemory_entity.UpdateSequence cs

  let string_of_change =
    CORE_inmemory_entity.string_of_change string_of_replacement
end

(** The implementation of the operations over entities. *)
module Make (I : U) : S with type data = I.data = struct

  type data = I.data

  type change = I.change

  type t = (data, change) entity

  (** The following module implements on-the-disk replication
      of entity descriptors. *)
  module OTD = CORE_onthedisk_entity.Make (I)

  (* ****** *)
  (*  Pool  *)
  (* ****** *)

  (** There must be at most one instance of an entity.  We use a pool
      to maintain the set of alive entities. *)
  let (loaded, load) =
    let pool = IdHashtbl.create 13 in
    (IdHashtbl.get pool, IdHashtbl.add pool)

  (** [awake id reaction] loads the entity named [id] from the
      file system and instantiate it in memory. *)
  let wakeup id reaction =
    OTD.load id >>>= fun description ->
    (** Notice that the following sequence of operations are
        atomic w.r.t. the concurrency model. *)
    let channel, push = CORE_client_reaction.channel () in
    let commit_lock = Lwt_mutex.create () in
    let commit_cond = Lwt_condition.create () in
    let e = {
      description; reaction; state = UpToDate;
      channel; push;
      commit_lock; commit_cond;
      mode = `Observe 0;
      log = Queue.create ()
    } in
    load id e;
    List.iter (register_dependency e) (dependency_image (dependencies e));
    return (`OK e)

  (** [initialize init deps fnames id] creates the on-the-disk
      representation of [id] so that it can be loaded afterwards.
      Precondition: [id] must not already exist. *)
  let initialize init deps properties fnames id =
    if OTD.exists id then
      return (`KO (`AlreadyExists (path_of_identifier id)))
    else
      OTD.save (CORE_inmemory_entity.make id deps properties fnames init)

  (* ************************** *)
  (*  Operations over entities  *)
  (* ************************** *)

  (** [make init reaction id] deals with the instanciation of [id] ... *)
  let rec make ?init ?(reaction = I.react) id =
    match init with
      | Some (init, dependencies, properties, filenames) ->
        (** This is the first time for [id], we make room for it in
            the file system ... *)
        initialize init dependencies properties filenames id
        (** ... and we instanciate it from that. *)
        >>>= fun () -> make ~reaction id

      | None ->
        (** [id] already exists somewhere ...*)
        match loaded id with
          (** ... in memory, we return this unique instance. *)
          | Some e -> return (`OK e)
          (** ... on the disk, we load it from the file system. *)
          | None -> wakeup id reaction

  let wait_to_be_observer_free e commit =
    Lwt_mutex.with_lock e.commit_lock (fun () ->
      (** Set the commit mode, no observers are allowed to run. *)
      e.mode <- `Commit;
      (** Nobody is watching! Do your stuff! *)
      commit ()
      >> return (e.mode <- `Observe 0)
    )

  (** [apply deps e c] does the effective changes [cs] of the
      content of [e]. *)
  let rec apply dependencies e cs =

    (** Notice that we globally maintain the invariant that two
        reactions cannot run concurrently on the same entity. So,
        during the execution of a reaction the current content of the
        entity cannot change.

        Furthermore, even if the actual modification of the entity
        state is done when no observer is watching, the reaction
        itself can be computed concurrently to observations: as soon
        as nothing is observable from the point of view of these
        observations, this is fine. *)

    e.reaction (identifier e) dependencies cs >>= function
      | NoUpdate ->
        return ()

      | llc ->
        wait_to_be_observer_free e (fun () ->
          let old = e.description in
          e.description <- CORE_inmemory_entity.update e.description llc;
          OTD.save e.description
          >>= function
            | `KO error -> warn error; return (e.description <- old)
            | `OK _ -> savelog e cs >>> return (e.push HasChanged)
        )

  and savelog e cs =
    let ts = CORE_inmemory_entity.timestamp e.description in
    List.iter (fun c -> Queue.push (c, ts) e.log) cs;
    let extra = Queue.length e.log - CORE_config.size_of_entity_log_history in
    let b = Buffer.create in
    if extra > 0 then
      for i = 0 to extra - 1 do
        try
          let (c, ts) = Queue.take e.log in
          Buffer.add_string b (
            Printf.sprintf "%f: %s\n" ts (string_of_change c)
          )
        with Queue.Empty -> assert false
      done;
    OTD.log e.identifier (Buffer.contents b)

  and log k e =
    let rec take accu = function
      | 0 -> accu
      | k -> try take (pred k) (Queue.peek e.log) with Queue.Empty -> accu
    in
    take k []

  (** [update e] is the process that applies scheduled changes. *)
  and update e =
    match e.state with
      | UpToDate ->
        return ()

      | Modified (dependencies, queue) ->
        let cs = Queue.fold (fun cs c -> c :: cs) [] queue in
        Queue.clear queue;
        apply dependencies e (List.rev cs)
        >> return (e.state <- UpToDate)

  (** As long as a change is being applied, we have to
      push other required changes into a queue. *)
  and now_only_accumulate_changes e =
    e.state <- Modified (empty_dependencies, Queue.create ())

  and change ?(immediate = false) e c =
    (** Shake the reverse dependencies for them to wait for
        a change of [e]. *)
    propagate_change (identifier e);

    match e.state with
      | UpToDate ->
        (** Good, the change is applied immediately. *)
        now_only_accumulate_changes e;
        apply empty_dependencies e [c]
        >> update e

      | Modified (dependencies, queue) ->
        (** This change is scheduled for further application. *)
        Queue.push c queue;
        if immediate then update e else return ()

  let observe (type a) (e : t) (o : data meta -> a Lwt.t) : a Lwt.t =
    (** We want the content to be as much fresh as possible. *)
    let rec try_to_observe () =
      update e >> (
        (let master = ref false in
         (if Lwt_mutex.is_locked e.commit_lock then
             match e.mode with
               | `Commit ->
                 (** A commit is being applied, wait for it to finish. *)
                 Lwt_condition.wait ~mutex:e.commit_lock e.commit_cond
                 (** and try to observe again. *)
                 >> try_to_observe ()

               | `Observe x -> return (
                 (** An observer already took the mutex. *)
                 assert (x > 1);
                 (** Let us register our presence to him. *)
                 e.mode <- `Observe (x + 1)
               )
          else (
            (** Neither a change nor an observer is working on that entity. *)
            assert (e.mode = `Observe 0);

            (** Entity, you are now observed: you cannot change while I am
                watching you! *)
            e.mode <- `Observe 1;

            (** This observer is called the "master" observer. *)
            master := true;

            (** The master observer owns the lock and it will release it
                only when no more observation is required. This model
                clearly gives priority to observations over changes,
                which is a good property for user-reactivity.  Yet, we
                will have to be careful not to totally starve changes
                processes...

                If I am correct, the fact that an observation first
                calls [update] for changes to be applied prevents an
                unbound creation of observers that could blocked the
                effective application of changes.
            *)
            Lwt_mutex.lock e.commit_lock
          ))
         >> (
           (** At this point, the lock is taken and the mode is to observe. *)
           assert (Lwt_mutex.is_locked e.commit_lock);
           assert (match e.mode with `Observe _ -> true | _ -> false);

           (** A good place for the observation to take place. *)
           lwt ret = o e.description in

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
                     >> wait_for_other_observers ()
                   | `Observe x -> assert false
                   | `Commit -> assert false
               in
               wait_for_other_observers ()

            else (

              (** For others, it simply means to quit the master. *)
              match e.mode with
                | `Observe x when x > 1 -> return (
                  e.mode <- `Observe (x - 1);
                  Lwt_condition.signal e.commit_cond ()
                )
                | `Observe _ -> assert false
                | `Commit -> assert false
            )
           ) >> return ret
         )
        )
      )
    in
    try_to_observe ()

  let identifier = identifier

end

(* ************** *)
(*  Unit tests.   *)
(* ************** *)

module Tests = struct

  type t = {
    log   : string list;
    count : int;
  } deriving (Json)

  module DummyEntity = Make (struct

    type data = t deriving (Json)

    let react _ _ deps new_content old_content =
      let (count_log, count) =
        match new_content with
          | None ->
            (Printf.sprintf "= %02d" old_content.count, old_content.count)
          | Some nc ->
            (Printf.sprintf "+ %02d" nc.count, nc.count)
      in
      let count = count + 1 in
      let deps_log =
        let ping_log rel ids id =
          Printf.sprintf "? %s (%s) = %s"
            rel
            (String.concat ", " (List.map string_of_identifier ids))
            (string_of_identifier id)
        in
        List.flatten (List.map (fun (label, fs) ->
          List.map (fun (ids, id) -> ping_log label ids id) fs
        ) (to_list deps))
      in
      let log s = Printf.sprintf "[%010f] %s" (Unix.gettimeofday ()) s in
      return (Some {
        log = List.map log (count_log :: deps_log);
        count
      })

  end)

  module E = DummyEntity

  let create_entity
      ?(dependencies = empty_dependencies)
      ?(properties = CORE_property.empty)
      ?(sources = [])
      update =
    let dummy = fresh tests_path "dummy" in
    let sdummy = string_of_identifier dummy in
    update (I18N.String.(create entity sdummy));
    E.make
      ~init:({ log = []; count = 0 }, dependencies, properties, sources)
      dummy
    >>= function
      | `OK e ->
        update (I18N.String.(created entity sdummy));
        return (`OK e)
      | `KO e ->
        return (`KO e)

  let already_there e update =
    E.make (identifier e)
    >>>= fun e' -> do_not_fail (fun () -> assert (e == e'))

  let observe_entity e update =
    lwt log = E.observe e (fun d -> return d.log) in
    List.iter update log;
    return (`OK ())

  let change_entity e update =
    E.change e (fun c ->
      return (Some { c with count = c.count + 1 })
    )
    >> return (`OK ())

  let echo_entity e update =
    let dependencies =
      of_list [ ("echo", [ ([], identifier e) ]) ]
    in
    create_entity ~dependencies update >>>= fun e' ->
    change_entity e update >>>= fun _ ->
    E.observe e' (fun d ->
      update (Printf.sprintf "%s.count = %d"
                (string_of_identifier (identifier e'))
                d.count);
      return d.count
    ) >>= fun c -> do_not_fail (fun () -> assert (c = 1))

  let check update =
    create_entity update
    >>>= fun e -> already_there e update
    >>>= fun _ -> change_entity e update
    >>>= fun _ -> observe_entity e update
    >>>= fun _ -> change_entity e update
    >>>= fun _ -> observe_entity e update
    >>>= fun _ -> echo_entity e update

end

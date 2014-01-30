(* -*- tuareg -*- *)

(** The reactive entities model. *)

open Lwt

open CORE_identifier
open CORE_standard_identifiers
open CORE_inmemory_entity
open CORE_error_messages
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
    'a meta                  (** with ['a meta] state may react to *)
    -> dependencies          (** a change of one of its dependencies ... *)
    -> 'c list               (** or to external requests to change ... *)
    -> ('c -> unit Lwt.t)    (** by scheduling a change or ... *)
    -> 'a state_change Lwt.t (** by requesting an immediate internal change. *)

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
  (*   *) react_cond  : unit Lwt_condition.t;
  (*   *) commit_lock : Lwt_mutex.t;
  (*   *) commit_cond : unit Lwt_condition.t;
  mutable mode        : [ `Commit | `Observe of int ];
  mutable log         : ('c * timestamp) list;
  mutable last_save   : timestamp
}

(** Shortcut for the type of entities. *)
and ('a, 'c) t = ('a, 'c) entity

(** Some data structures contain entities with heterogeneous
    content types. In that case, this type is hidden behind
    an existential quantification. *)
type some_t = SomeEntity : ('a, 'c) t -> some_t

(** Accessor to the unique identifier of the entity. *)
let identifier e = CORE_inmemory_entity.identifier e.description

(** Check the existence of an entity. *)
let exists id = CORE_onthedisk_entity.exists id

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
let table = Hashtbl.create 13
let channel e =
  let eid = identifier e in
  try
    Hashtbl.find table eid
  with Not_found ->
    let c =
      Eliom_comet.Channel.create_newest e.channel
    in
    Hashtbl.add table eid c;
    c

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

  val change
    : ?who:identifier -> t -> change -> unit Lwt.t

  val observe
    : ?who:identifier -> t -> (data meta -> 'a Lwt.t) -> 'a Lwt.t

  val identifier : t -> CORE_identifier.t

  val log : t -> int -> (change * timestamp) list

  val source_filename : t -> string -> string
end

(** The client must provide the following information
    about his specific entity. *)
module type U = sig
  type data deriving (Json)
  type change
  val react : (data, change) reaction
  val string_of_change : change -> string
  val current_version : string
  val converters : (module CORE_onthedisk_entity.Converter with type destination = data) list
end

module type D =
sig
  type data deriving (Json)
  val string_of_replacement : data -> string
  val current_version : string
  val converters : (module CORE_onthedisk_entity.Converter with type destination = data) list
end

module Passive (I : D) : U
  with type data = I.data
  and type change = I.data CORE_inmemory_entity.state_change
= struct
  type data = I.data deriving (Json)
  type change = I.data CORE_inmemory_entity.state_change

  let react _ _ cs _ =
    return (CORE_inmemory_entity.state_changes cs)

  let string_of_change =
    CORE_inmemory_entity.string_of_state_change I.string_of_replacement

  let current_version = I.current_version
  let converters = I.converters
end

(** The implementation of the operations over entities. *)
module Make (I : U) : S
with type data = I.data
and type change = I.change
= struct

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
  let (loaded, load, iter_on_pool) =
    let pool = IdHashtbl.create 13 in
    (IdHashtbl.get pool, IdHashtbl.add pool, fun f -> IdHashtbl.iter f pool)

  (** [awake id reaction] loads the entity named [id] from the
      file system and instantiate it in memory. *)
  let rec wakeup id reaction =
    OTD.load id >>>= fun description ->
    (** Notice that the following sequence of operations are
        atomic w.r.t. the concurrency model. *)
    let channel, push = CORE_client_reaction.channel () in
    let react_cond = Lwt_condition.create () in
    let commit_lock = Lwt_mutex.create () in
    let commit_cond = Lwt_condition.create () in
    let e = {
      description; reaction; state = UpToDate;
      channel; push; react_cond;
      commit_lock; commit_cond;
      mode = `Observe 0;
      log = [];
      last_save = 0.
    } in
    load id e;
    List.iter (register_dependency e)
      (dependency_image (dependencies e.description)
      );
    Lwt.async (fun () ->
      let rec tick () =
        (** This is the only place where update is called. *)
        update e
        (* FIXME: I would like to use the following condition to
           FIXME: this active loop. Yet, for some reason, it does
           FIXME: work. For the moment, I stay with an active loop
           FIXME: because it is not expansive but this is clearly
           FIXME: not satisfactory... *)
        (*        >> Lwt_condition.wait e.react_cond *)
        >> Lwt_unix.yield ()
        >> Lwt_unix.sleep 1.
        >>= tick
      in
      tick ()
    );
    return (`OK e)

  (** [initialize init deps fnames id] creates the on-the-disk
      representation of [id] so that it can be loaded afterwards.
      Precondition: [id] must not already exist. *)
  and initialize init deps properties fnames id =
    if CORE_onthedisk_entity.exists id then
      return (`KO (`AlreadyExists (path_of_identifier id)))
    else
      OTD.save (CORE_inmemory_entity.make id deps properties fnames init)

  (* ************************** *)
  (*  Operations over entities  *)
  (* ************************** *)

  (** [make init reaction id] deals with the instanciation of [id] ... *)
  and make ?init ?(reaction = I.react) id =
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

  (** [apply deps e c] does the effective changes [cs] of the
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
    Ocsigen_messages.errlog (
      Printf.sprintf "%s is reacting (%d changes : %s)"
        (string_of_identifier (identifier e))
        card_changes
        (String.concat " " (List.map I.string_of_change cs))
    );
    if card_changes = 0 then
      return ()
    else
      e.reaction state dependencies cs change_later >>= function
        | NoUpdate ->
          return ()

        | llc ->
          wait_to_be_observer_free e (fun () ->
            let old = e.description in
            e.description <- CORE_inmemory_entity.update e.description llc;
            (* FIXME: The following optimization is unsafe: we must ensure *)
            (* FIXME: that every entity is finally saved. *)
            save_on_disk e >>= function
              | `KO error ->
                warn error;
                return (e.description <- old)
              | `OK _ -> savelog e cs >>= fun _ ->
                e.push HasChanged;
                return ()
          ) >>= fun () -> return (propagate_change (identifier e))

  and save_on_disk e =
    (* FIXME: 60. must be a parameter. *)
    if (timestamp e.description) -. e.last_save >= 60. then begin
      e.last_save <- timestamp e.description;
      OTD.save e.description
    end
    else return (`OK ())

  and savelog e cs =
    let ts = CORE_inmemory_entity.timestamp e.description in
    let ls = List.map (fun c -> (c, ts)) cs in
    e.log <- ls @ e.log;
    let extra = List.length e.log - CORE_config.size_of_entity_log_history in
    if extra > 0 then e.log <- COMMON_pervasives.list_tl_cut extra e.log;
    let b = Buffer.create 31 in
    List.iter (fun (c, ts) ->
      Buffer.add_string b (Printf.sprintf "%f: %s\n" ts (I.string_of_change c))
    ) ls;
    Ocsigen_messages.errlog (Buffer.contents b);
    CORE_onthedisk_entity.log (identifier e) (Buffer.contents b)

  and log e k =
    COMMON_pervasives.list_take k e.log

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

  let observe (type a) ?who (e : t) (o : data meta -> a Lwt.t)
      : a Lwt.t =
    let who = match who with
      | None -> "anonymous"
      | Some id -> string_of_identifier id
    in
    let say msg =
      if false then
        Ocsigen_messages.errlog (Printf.sprintf "%s observes %s: %s"
                                   who
                                   (string_of_identifier (identifier e)) msg)
    in
    let master = ref false in
    say "";
      let rec aux () =
        say (Printf.sprintf "%s trying to be observed"
               (string_of_identifier (identifier e)));

        let incr_observers () =
          match e.mode with
            | `Commit ->
              bad_assumption "Observing mode."
            | `Observe x -> e.mode <- `Observe (x + 1)
        in
        if Lwt_mutex.is_locked e.commit_lock
          || not (Lwt_mutex.is_empty e.commit_lock)
        then
          match e.mode with
            | `Commit ->
              (** A commit is being applied, wait for it to finish. *)
              say (Printf.sprintf "%s: Observer waits commit"
                                         (string_of_identifier (identifier e)));
              Lwt_condition.wait e.commit_cond
              (** and try to observe again. *)
              >>= aux

            | `Observe x -> return (
              say (Printf.sprintf "%s: is observed"
                                         (string_of_identifier (identifier e)));

                (** An observer already took the mutex. *)
                (** Let us register our presence to him. *)
              incr_observers ()
            )
        else (
          say
            (Printf.sprintf "Waiting for lock (%B, %B)"
               (Lwt_mutex.is_locked e.commit_lock)
               (Lwt_mutex.is_empty e.commit_lock));

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
           with e ->
             bad_assumption (
               Printf.sprintf
                 "Observers do not raise exceptions (Here: %s)."
                 (Printexc.to_string e)
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
                   say "Unlock";
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
                  bad_assumption (
                    Printf.sprintf "Still (%d >= 1) when slave observer stops"
                      x
                  );
                e.mode <- `Observe (x - 1);
                Lwt_condition.signal e.commit_cond ()
              )
              | `Commit ->
                bad_assumption
                  "Entity can be in commit mode during observation.";
                return ()
          )
         )
         >>= fun () -> (
           say
             (Printf.sprintf "Observation done (master=%B)!" !master);
           return ret
         )
      )

  let identifier = identifier

  let source_filename x =
    CORE_standard_identifiers.source_filename (identifier x)

  let _ =
    Lwt.async (fun () ->
      let rec forever () =
        return (
          iter_on_pool (fun id e ->
            Lwt.async (fun () -> save_on_disk e >>= fun _ -> return ()))
        )
        >> Lwt_unix.sleep 60.
        >> forever ()
      in
      forever ()
    )

end

(* ************** *)
(*  Unit tests.   *)
(* ************** *)

module Tests = struct

  type t = {
    log   : string list;
    count : int;
  } deriving (Json)

  type public_change = Incr

  module DummyEntity = Make (struct

    type data = t deriving (Json)

    let current_version = "test"
    let converters = []

    type change = public_change

    let string_of_change _ = "Increment."

    let react state deps cs change_later =
      let count =
        List.fold_left (fun count _ -> count + 1) (content state).count cs
      in
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
      return (UpdateContent {
        log = List.map log deps_log;
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
    lwt log = E.observe e (fun d -> return (content d).log) in
    List.iter update log;
    return (`OK ())

  let change_entity e update =
    E.change e Incr >>= fun () -> return (`OK ())

  let echo_entity e update =
    let dependencies =
      of_list [ ("echo", [ ([], identifier e) ]) ]
    in
    create_entity ~dependencies update >>>= fun e' ->
    change_entity e update >>>= fun _ ->
    E.observe e' (fun d ->
      update (Printf.sprintf "%s.count = %d"
                (string_of_identifier (identifier e'))
                (content d).count);
      return (content d).count
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

(* -*- tuareg -*- *)

(** The reactive entities model. *)

open Lwt

open CORE_identifier
open CORE_inmemory_entity
open COMMON_pervasives

(* ****************** *)
(*  Type definitions  *)
(* ****************** *)

type 'a reaction = dependencies -> 'a option -> 'a change

and 'a change = 'a -> 'a Lwt.t

type 'a state =
| UpToDate
| Modified of dependencies * 'a change Queue.t

type 'a entity = {
  mutable description : 'a meta;
  mutable state       : 'a state;
  (*   *) reaction    : 'a reaction;
}

type 'a t = 'a entity

type some_t = SomeEntity : 'a t -> some_t

let dependencies e = CORE_inmemory_entity.dependencies e.description

let identifier e = CORE_inmemory_entity.identifier e.description

(* ********************** *)
(*  Reverse dependencies  *)
(* ********************** *)

module EntitySet = Hashtbl.Make (struct
  type t = some_t
  let hash (SomeEntity x) = Hashtbl.hash (identifier x)
  let equal (SomeEntity x) (SomeEntity y) =
    CORE_identifier.compare (identifier x) (identifier y) = 0
end)

module Watchers = struct
  include Hashtbl.Make (CORE_identifier)
  let get h k = try Some (find h k) with Not_found -> None
end

type watchers = some_t EntitySet.t Watchers.t

let watchers = Watchers.create 13

(** [watchers_of id] returns the set of all reverse
    dependencies of [id]. *)
let watchers_of id =
  try
    Watchers.find watchers id
  with Not_found -> EntitySet.create 13

(** [e] depends on [e']. *)
let register_dependency e id =
  EntitySet.add (watchers_of id) (SomeEntity e) ()

(* *********************** *)
(*  Instantiation functor  *)
(* *********************** *)

module type S = sig
  type data
  type t = data entity
  val make:
    ?init:(data * dependencies)
    -> ?reaction:(data reaction)
    -> CORE_identifier.t ->
    [ `OK of t
    | `KO of [
      | `UndefinedEntity of CORE_identifier.t
      | `AlreadyExists   of CORE_identifier.t
      | `SystemError     of string
    ]] Lwt.t

  val change  : t -> data change -> unit Lwt.t
  val observe : t -> (data -> 'a Lwt.t) -> 'a Lwt.t
end

module type U = sig
  type data deriving (Json)
  val react : data reaction
end

let passive _ _ x = return x

module Make (I : U) : S with type data = I.data = struct

  type data = I.data

  type t = data entity

  module OTD = CORE_onthedisk_entity.Make (I)

  (* ****** *)
  (*  Pool  *)
  (* ****** *)

  type pool = t Watchers.t

  let create_pool () = Watchers.create 13

  let loaded pool id = Watchers.get pool id

  let load pool id e = Watchers.add pool id e

  let alive pool id reaction =
    OTD.load id >>= function
      | `OK description ->
        let e = { description; reaction; state = UpToDate } in
        load pool id e;
        List.iter (register_dependency e) (dependency_image (dependencies e));
        return (`OK e)
      | `KO (`UndefinedEntity e) ->
        return (`KO (`UndefinedEntity e))
      | `KO (`SystemError e) ->
        return (`KO (`SystemError e))

  let initialize init dependencies reaction id =
    if OTD.exists id then
      return (`KO (`AlreadyExists id))
    else
      let data = CORE_inmemory_entity.make id dependencies init in
      OTD.save data

  (* ************************** *)
  (*  Operations over entities  *)
  (* ************************** *)

  let rec make pool ?init reaction id =
    match init with
      | Some (init, dependencies) ->
        initialize init dependencies reaction id
        >>>= fun () -> make pool reaction id

      | None ->
        match loaded pool id with
          | Some e ->
            return (`OK e)

          | None ->
            alive pool id reaction

  let pool = create_pool ()
  let make ?init ?(reaction = I.react) = make pool ?init reaction

  let apply dependencies e c =
    let content0 = content e.description in
    lwt content' = c content0 in
    lwt content  = e.reaction dependencies (Some content') content0 in
    return (e.description <- update_content e.description content)

  let rec update e =
    match e.state with
      | UpToDate ->
        return ()
      | Modified (dependencies, queue) ->
        let rec flush () =
          try
            let c = Queue.take queue in
            apply dependencies e c >> flush ()
          with Queue.Empty ->
            return (e.state <- UpToDate)
        in
        flush ()

  let now_only_accumulate_changes e =
    e.state <- Modified (empty_dependencies, Queue.create ())

  let change e c =
    match e.state with
      | UpToDate ->
        now_only_accumulate_changes e;
        apply empty_dependencies e c >> update e

      | Modified (dependencies, queue) ->
        return (Queue.push c queue)

  let observe e o =
    update e >> (
      now_only_accumulate_changes e;
      o (content e.description)
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

  module DummyEntity = Make (struct

    type data = t deriving (Json)

    let react deps new_content old_content =
      let (count_log, count) =
        match new_content with
          | None ->
            (Printf.sprintf "= %02d" old_content.count, old_content.count)
          | Some nc ->
            (Printf.sprintf "+ %02d" nc.count, nc.count)
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
      return {
        log = List.map log (count_log :: deps_log);
        count
      }

  end)

  module E = DummyEntity

  let create_entity update =
    let dummy = CORE_identifier.fresh CORE_identifier.tests_path "dummy" in
    E.make ~init:({ log = []; count = 0 }, empty_dependencies) dummy
    >>= function
      | `OK e ->
        update (I18N.String.created (string_of_identifier dummy));
        return (`OK ())
      | `KO e ->
        return (`KO e)


  let check update =
    create_entity update

end

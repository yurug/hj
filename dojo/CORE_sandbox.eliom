(** -*- tuareg -*- *)

(** A sandbox is a runtime UNIX environment that has a restricted
    set of resources and facilities. A sandbox can be persistent,
    which means that its state will survive between two jobs.

    From an implementation point of view, the problem is to properly
    manage a set of (physical or virtual) machines to respond to
    the job requests.
*)

open Lwt
open COMMON_pervasives
open CORE_error_messages

{shared{

type job = int deriving (Json)

}}

type observable =
  | WaitingForSandbox of int
  | WriteStdout       of string
  | WriteStderr       of string
  | FileModification  of string * (unit -> string Lwt.t)
  | Exited            of int

type sandbox_id = int

let release id =
  (* FIXME *)
  return ()

type persistence =
  | Ephemeral
  | RequirePersistence
  | Persistent of sandbox_id

type limitation =
  | TimeOut of int

type requirement =
  | SucceedingCommand of string
  | Is of string
  | Lockable

(** [find_sandbox requirements] allocates a sandbox that fullfills the
    [requirements]. If this allocation is impossible, raise
    [NoSuchSandbox]. *)
exception NoSuchSandbox
let find_sandbox requirements =
  (** Iterate over all the machinists, looking for one that
      provides sandboxes that fullfill the [requirements]. *)
  lwt machinists = CORE_machinist.all () in
  List.iter (fun m ->
    Ocsigen_messages.errlog (
      Printf.sprintf "Try %s"
        (CORE_identifier.string_of_identifier (CORE_machinist.identifier m))))
    machinists;
  raise NoSuchSandbox

(** [copy files sandbox] imports the files in the sandbox. *)
let copy files sandbox =
  return ()

(** [sandbox_exec limitations command observer] *)
let sandboxing limitations command observer =
  return 0

(** [exec ?persistent ?limitations files command observer] first
    copies [files] from the server to the sandbox, then executes
    [command] asynchronously with some [limitations] and immediately
    returns a job descriptor as well as a persistence descriptor.  *)
let exec
    ?(persistence = Ephemeral) ?(limitations = []) ?(requirements = [])
    files command observer =

  try

    let requirements =
      cons_if (persistence = RequirePersistence) Lockable requirements
    in

    (** Determine the sandbox to play with. *)
    lwt sandbox_id =
      match persistence with
        | Ephemeral | RequirePersistence ->
          find_sandbox requirements
        | Persistent id ->
          return id
    in
    let persistence = match persistence with
      | RequirePersistence -> Persistent sandbox_id
      | p -> p
    in

    (** Process command. *)
    copy files sandbox_id
    >> sandboxing limitations command observer
    >>= fun job -> return (`OK (job, persistence))

  with NoSuchSandbox ->
    let e = `NoSuchSandbox in
    warn e;
    return (`KO e)

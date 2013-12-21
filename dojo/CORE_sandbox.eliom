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
open CORE_identifier

let jobs = Hashtbl.create 13

let new_job () =
  let rec push () =
    (* FIXME: Probabilistic assumption: are we sure that we
       will never try to cancel a running job using an old
       reference that has the same id? *)
    let idx = Random.bits () in
    if Hashtbl.mem jobs idx then push () else idx
  in
  let idx = push () in
  Hashtbl.add jobs idx (fun () -> ());
  idx

let push_canceler = Hashtbl.replace jobs

{shared{

type job = int deriving (Json)

let string_of_job = string_of_int

}}

let job_canceler job =
  try
    Hashtbl.find jobs job
  with
    | Not_found ->
      (** This means that this job is already over. *)
      fun () -> ()

type observable =
  | WaitingForSandbox of int
  | WriteStdout       of job * string
  | WriteStderr       of job * string
  | FileModification  of string * (unit -> string Lwt.t)
  | Exited            of Unix.process_status

let release id =
  (* FIXME *)
  return ()

type sandbox = CORE_machinist.sandbox_interface

type persistence =
  | Ephemeral
  | RequirePersistence
  | Reuse of sandbox

type limitation =
  | TimeOut of float

type requirement =
  | SucceedingCommand of string
  | Is of string
  | Lockable

(** [find_sandbox requirements] allocates a sandbox that fullfills the
    [requirements]. If this allocation is impossible, raise
    [NoSuchSandbox]. *)
exception NoSuchSandbox
let find_sandbox requirements waiter =
  (** Iterate over all the machinists, looking for one that
      provides sandboxes that fullfill the [requirements]. *)
  lwt machinists = CORE_machinist.all () in
  let compliant_machinist m =
    let check_requirement = function
      | SucceedingCommand cmd ->
        CORE_machinist.capable_of m cmd
      | Is who ->
        return (string_of_identifier (CORE_machinist.identifier m) = who)
      | Lockable ->
        CORE_machinist.lockable m
    in
    Lwt_list.for_all_s check_requirement requirements
  in
  let lock_required = List.exists (fun r -> r = Lockable) requirements in
  try_lwt
    lwt mc = Lwt_list.find_s compliant_machinist machinists in
    Ocsigen_messages.errlog (Printf.sprintf "Asking %s\n" (string_of_identifier (CORE_machinist.identifier mc)));
    CORE_machinist.provide_sandbox_interface mc lock_required waiter
  with Not_found ->
    raise_lwt NoSuchSandbox

(** [copy files sandbox] imports the files into the sandbox. *)
let copy files sandbox =
  (* FIXME *)
  return ()

let on_line oc w = Lwt.async (fun () ->
  try_lwt
    Lwt_stream.iter_s w (Lwt_io.read_lines oc)
  with _ ->
    (** We stop the process when the stream is not alive anymore. *)
    return ()
)

(** [sandboxing sb limitations command observer] *)
let sandboxing release_flag s limitations command (observer : _ -> unit Lwt.t) =
  let job = new_job () in
  let timeout =
    List.fold_left (fun _ (TimeOut t) -> Some t) None limitations
  in
  let observer = function
    | CORE_machinist.ObserveProcess p ->
      on_line p#stdout (fun l -> observer (WriteStdout (job, l)));
      on_line p#stderr (fun l -> observer (WriteStderr (job, l)));
      lwt status = p#status in
      Ocsigen_messages.errlog "Exec finished";
      (if release_flag then s.CORE_machinist.release () else return ())
      >>= fun _ -> (
        Ocsigen_messages.errlog "Notify observer";
        observer (Exited status)
      )

    | CORE_machinist.ObserveMessage msg ->
      observer (WriteStderr (job, msg))
      >>= fun _ -> observer (Exited (Unix.WEXITED (-1)))
  in
  lwt canceler = s.CORE_machinist.execute ?timeout command observer in
  push_canceler job canceler;
  return job

(** [exec ?persistent ?limitations files command observer] first
    copies [files] from the server to the sandbox, then executes
    [command] asynchronously with some [limitations] and immediately
    returns a job descriptor as well as a persistence descriptor.  *)
let exec
    ?(persistence = Ephemeral) ?(limitations = []) ?(requirements = [])
    files command observer =

  try_lwt

    let requirements =
      cons_if (persistence = RequirePersistence) Lockable requirements
    in

    (** Determine the sandbox to play with. *)
    lwt sandbox =
      match persistence with
        | Ephemeral | RequirePersistence ->
          let waiter = fun rank -> observer (WaitingForSandbox rank) in
          find_sandbox requirements waiter
        | Reuse i ->
          return i
    in
    let persistence = match persistence with
      | RequirePersistence -> Reuse sandbox
      | p -> p
    in
    let release_when_finished = persistence <> RequirePersistence in

    (** Process command. *)
    copy files sandbox
    >>= fun _ -> sandboxing release_when_finished sandbox limitations command observer
    >>= fun job -> return (`OK (job, persistence))

  with NoSuchSandbox ->
    let e = `NoSuchSandbox in
    warn e;
    return (`KO e)

let cancel job =
  job_canceler job ()

(** -*- tuareg -*- *)

(** A sandbox is a runtime UNIX environment that has a restricted
    set of resources and facilities. A sandbox can be persistent,
    which means that its state will survive between two jobs.
*)

{shared{

type job = int deriving (Json)

val string_of_job : job -> string

}}

type observable =
  | WaitingForSandbox of int
  | WriteStdout       of job * string
  | WriteStderr       of job * string
  | FileModification  of string * (unit -> string Lwt.t)
  | Exited            of Unix.process_status

type sandbox

val release : sandbox -> unit Lwt.t

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

(** [exec ?persistent ?limitations files command observer] first
    copies [files] from the server to the sandbox, then executes
    [command] asynchronously with some [limitations] and immediately
    returns a job descriptor as well as a persistence descriptor.  *)
val exec :
  ?persistence:persistence ->
  ?limitations:limitation list ->
  ?requirements:requirement list ->
  string list ->
  string ->
  (observable -> unit Lwt.t) -> [>
    `OK of (job * persistence)
  | `KO of [> `NoSuchSandbox ]
  ] Lwt.t

val cancel : job -> unit

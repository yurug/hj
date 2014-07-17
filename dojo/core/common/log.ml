type event = Event of event_identifier * timestamp * descriptor

and timestamp = float

and descriptor = term list

and term =
  | TApp    of symbol * term list
  | TInt    of int
  | TString of string
  | TEvent  of event_identifier

and symbol = Symbol of string

and event_identifier = int

let warning = Symbol "warning"
let endof   = Symbol "endof"

let logs = Queue.create ()

let id = ref 0

(* FIXME: is Unix.gettimeofday a syscall? Be aware of this possible
   FIXME: source of inefficiency... *)
let log d =
  incr id;
  Queue.push (Event (!id, Unix.gettimeofday (), d)) logs;
  !id

type warning = descriptor

let warning s = [TApp (warning, [TString s])]

let make_unary_string_event e = fun s -> [TApp (Symbol e, [TString e])]

let endof e = [TApp (endof, [TEvent e])]

let log_process d =
  let e = log d in
  (e, fun () -> log (endof e))

type event_identifier = int

type 'a ty =
  | TString : string ty
  | TInt    : int ty
  | TEvent  : event_identifier ty
  | TP      : 'a ty * 'b ty -> ('a * 'b) ty

type event = Event of event_identifier * timestamp * descriptor

and timestamp = float

and descriptor = term list

and term = Term : 'a symbol * 'a -> term

and 'a symbol = Symbol of string * 'a ty

let warning = Symbol ("warning", TString)
let endof   = Symbol ("endof", TEvent)

let logs = Queue.create ()

let id = ref 0

(* FIXME: is Unix.gettimeofday a syscall? Be aware of this possible
   FIXME: source of inefficiency... *)
let log d =
  incr id;
  Queue.push (Event (!id, Unix.gettimeofday (), d)) logs;
  !id

let log' d = ignore (log d)

type warning = descriptor

let descriptor_maker s ty x = [Term (Symbol (s, ty), x)]

let make_unary_string_event e = descriptor_maker e TString

let warning s = [Term (warning, s)]

let endof e = [Term (endof, e)]

let log_process d =
  let e = log d in
  (e, fun () -> log (endof e))

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

let logs = Queue.create ()

let id = ref 0

let log d =
  incr id;
  Queue.push (Event (!id, Unix.gettimeofday (), d)) logs;
  !id

type warning = descriptor

let warning s = [TApp (warning, [TString s])]

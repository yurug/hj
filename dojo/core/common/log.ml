type event = Event of timestamp * descriptor

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

let log d = Queue.push (Event (Unix.gettimeofday (), d)) logs

type warning = descriptor

let warning s = [TApp (warning, [TString s])]

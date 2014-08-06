open Facts

type 'a event_descriptor = (string * 'a) predicate * string

let make_event_descriptor s ty =
  let ty = string ** ty in
  (make_predicate "event" True True ty, s)

let end_of_process = make_predicate "end_of_process" True True TStatement

let log who (p, s) x = ignore (state who p (s, x))

let log_process who (p, s) x =
  let start = state who p (s, x) in
  fun () -> state who end_of_process start

let warn_only s = assert false

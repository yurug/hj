type t = float

type timestamp = t

let current () = Unix.gettimeofday ()

let origin () = 0.

let compare : t -> t -> int = compare

type interval = t * t

let make start stop = (start, stop)

let mem x (start, stop) = start <= x && x <= stop

let always = (0., infinity)

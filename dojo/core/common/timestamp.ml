type t = float deriving (Json)

type timestamp = t deriving (Json)

(* FIXME: use clock_gettime or an equivalent to get
   a strictly monotonic clock.  *)
let current () = Unix.gettimeofday ()

let origin () = 0.

let compare : t -> t -> int = compare

type interval = t * t

let make start stop = (start, stop)

let mem x (start, stop) = start <= x && x <= stop

let always = (0., infinity)

let older_than d t = current () -. t > d

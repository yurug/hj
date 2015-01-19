type t = float deriving (Json)

type timestamp = t deriving (Json)

let to_string f = Unix.(
  let tm = localtime f in
  Printf.sprintf
    "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.tm_year + 1900) (1 + tm.tm_mon) (1 + tm.tm_mday)
    tm.tm_hour tm.tm_min tm.tm_sec
)

let shift t s = t +. s

(* FIXME: use clock_gettime or an equivalent to get
   a strictly monotonic clock.  *)
let current () = Unix.gettimeofday ()

let origin () = 0.

let compare : t -> t -> int = compare

type interval = t * t

let make start stop = (start, stop)

let mem x (start, stop) = start <= x && x <= stop

let always = (0., infinity)

let older_than t1 t2 = t2 -. t1 > 0.

let older_than_duration d t = current () -. t > d

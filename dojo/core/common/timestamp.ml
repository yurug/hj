type t = float

type timestamp = t

let current () = Unix.gettimeofday ()

let compare : t -> t -> int = compare

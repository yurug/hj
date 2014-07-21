type t = float

type timestamp = t

let now () = Unix.gettimeofday ()

let compare : t -> t -> int = compare

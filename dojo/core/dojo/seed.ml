type t = int

let generate () = Random.bits ()

let to_string = string_of_int

let of_string = int_of_string

let compare = compare

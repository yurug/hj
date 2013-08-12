(* -*- tuareg -*- *)

let forever what =
  let rec aux () = what aux in aux ()

(* -*- tuareg -*- *)

{shared{

type command =
  | Empty
  | PushLine of string
  | Seq of command * command
deriving (Json)

let merge c1 c2 = Seq (c1, c2)

let rec string_of_command = function
  | Empty -> "0"
  | PushLine s -> "say(" ^ s ^ ")"
  | Seq (c1, c2) -> string_of_command c1 ^ ";" ^ string_of_command c2

}}

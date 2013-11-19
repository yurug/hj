(* -*- tuareg -*- *)

{shared{

type command =
  | Empty
  | PushLine of string
  | Seq of command * command
deriving (Json)

let merge c1 c2 = Seq (c1, c2)

}}

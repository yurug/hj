(* -*- tuareg -*- *)

{shared{

type command =
  | Reset
  | PushLine of string
deriving (Json)

}}

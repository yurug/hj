(* -*- tuareg -*- *)

(** Autotesting service (disabled by default). *)

{shared{

type test_result =
  | Passed
  | Failed
deriving (Json)

}}

type test

val description : test -> string

type reporter = string -> unit

val run : test -> reporter -> test_result Lwt.t

val all : test list

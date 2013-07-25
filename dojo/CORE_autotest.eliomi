(* -*- tuareg -*- *)

(** Autotesting service (disabled by default). *)

type test_result =
  | Passed
  | Failed

type test

val description : test -> string

val run : test -> test_result Lwt.t

val all : test list

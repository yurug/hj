(* -*- tuareg -*- *)

(** Errors. *)

type all = [
| `AlreadyExists         of CORE_identifier.path
| `DirectoryDoesNotExist of CORE_identifier.path
| `SystemError           of string
| `UndefinedEntity       of CORE_identifier.t
| `AssertFailure         of string
]

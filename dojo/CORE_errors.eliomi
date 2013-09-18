(* -*- tuareg -*- *)

(** Errors. *)

type all = [
| `AlreadyExists         of CORE_identifier.path
| `DirectoryDoesNotExist of CORE_identifier.path
| `SystemError           of string
| `UndefinedEntity       of CORE_identifier.t
| `InvalidLabel          of string
| `AssertFailure         of string
| `MaximalNumberOfLoginAttemptsReached
| `BadLoginPasswordPair
]

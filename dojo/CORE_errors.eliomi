(* -*- tuareg -*- *)

(** Errors. *)
{shared{

type all = [
| `AlreadyExists         of CORE_identifier.path
| `DirectoryDoesNotExist of CORE_identifier.path
| `SystemError           of string
| `UndefinedEntity       of CORE_identifier.t
| `InvalidLabel          of string
| `AssertFailure         of string
| `SyntaxError           of Lexing.position * Lexing.position * string
| `MaximalNumberOfLoginAttemptsReached
| `BadLoginPasswordPair
| `NoSuchSandbox
]

}}

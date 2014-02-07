(* -*- tuareg -*- *)

(** Errors. *)
{shared{

type position =
    { filename : string; line : int; character : int }
deriving (Json)

val from_lexing_position : Lexing.position -> position

val to_lexing_position : position -> Lexing.position

exception ParseError of position * position * string

type all = [
| `AlreadyExists         of CORE_identifier.path
| `DirectoryDoesNotExist of CORE_identifier.path
| `SystemError           of string
| `UndefinedEntity       of CORE_identifier.t
| `InvalidLabel          of string
| `AssertFailure         of string
| `SyntaxError           of position * position * string
| `MaximalNumberOfLoginAttemptsReached
| `BadLoginPasswordPair
| `NoSuchSandbox
| `TypeError             of position * string
| `NeedAnnotation        of position
| `UnboundVariable       of position * string
| `BadApplication        of position
| `EvalError
] deriving (Json)

}}

(* -*- tuareg -*- *)

(** Errors. *)
{shared{

type position = { line : int; character : int } deriving (Json)

let from_lexing_position p =
  { line      = p.Lexing.pos_lnum;
    character = p.Lexing.pos_cnum - p.Lexing.pos_bol;
  }

let to_lexing_position p = Lexing.(
  { dummy_pos with
    pos_lnum = p.line;
    pos_cnum = 0;
    pos_bol = p.character
  }
)

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

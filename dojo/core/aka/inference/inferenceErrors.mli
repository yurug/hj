(** Turn exceptions into error messages for type elaboration. *)

open Position
open MultiEquation

(** [handle_error print_variable f] executes [f ()] and exits the
    program with an error messages if [f ()] raised an exception. Use
    [print_variable] to pretty print internal representation of
    types. *)
val handle_error : (position -> variable -> string) -> (unit -> 'a) -> 'a

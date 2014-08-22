(** Error messsages for the type checking of {!XAST} and its
    elaboration into ML. *)

(** [handle_error f] produces human-readable located error messages
    out of exceptions raised by [f ()]. *)
val handle_error : (unit -> 'a) -> 'a

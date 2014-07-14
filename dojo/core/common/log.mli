type event

type event_identifier

type descriptor

type warning = descriptor

val log : descriptor -> event_identifier

val warning : string -> descriptor

val make_unary_string_event : string -> (string -> descriptor)

val log_process : descriptor -> event_identifier * (unit -> event_identifier)

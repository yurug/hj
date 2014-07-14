type event

type event_identifier

type descriptor

type warning = descriptor

val log : descriptor -> event_identifier

val warning : string -> descriptor

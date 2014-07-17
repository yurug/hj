type event

type event_identifier

type descriptor

type warning = descriptor

val log : descriptor -> event_identifier

val log' : descriptor -> unit

val warning : string -> descriptor

type 'a ty =
  | TString : string ty
  | TInt    : int ty
  | TEvent  : event_identifier ty
  | TP      : 'a ty * 'b ty -> ('a * 'b) ty

val descriptor_maker : string -> 'a ty -> ('a -> descriptor)

val make_unary_string_event : string -> (string -> descriptor)

val log_process : descriptor -> event_identifier * (unit -> event_identifier)

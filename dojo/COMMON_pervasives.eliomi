(* -*- tuareg -*- *)

(** [forever that] does [that] for ever if [that] [continues ()] when
    applied to [continues]. *)
val forever : ((unit -> unit) -> unit) -> unit

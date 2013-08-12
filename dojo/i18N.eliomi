(* -*- tuareg -*- *)

(** Internalization. *)

(** [cap s] returns the capitalized version of [s]. *)
val cap : string -> string

(** The chosen set of messages. *)
module String : I18N_sig.Text

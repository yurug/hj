(* -*- tuareg -*- *)

(** Remote access of dynamically generated HTML fragments. *)

{shared{

type idx deriving (Json)

type elt = [ Html5_types.flow5 ] Eliom_content.Html5.D.elt

}}

{server{
(** [local_push elt] stores a fragment until it is requested. *)
val local_push : elt -> idx
}}

{client{
(** [remote_get idx] returns a remote fragment identified by [idx]. *)
val remote_get : (idx, elt) Eliom_pervasives.server_function
}}

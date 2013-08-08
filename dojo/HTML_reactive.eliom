(* -*- tuareg -*- *)

{shared{
open Lwt
open Eliom_content
open Html5.D
open Html5_types
type t = [ div ] elt
type 'a c = t * 'a CORE_client_reaction.c
}}

{client{
  let update_div parent to_div x =
    lwt elt = to_div x in
    return (Eliom_content.Html5.Manip.replaceAllChild parent [ elt ])

  let react (elt, bus) reaction =
    CORE_client_reaction.install_automatic_client_reaction bus (
      update_div elt reaction
    )
}}

let async_div json computation reaction =
  let elt = Html5.Id.create_global_elt (div []) in
  CORE_client_reaction.on json computation (fun bus -> reaction (elt, bus)) >>
  return elt

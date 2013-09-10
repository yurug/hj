(* -*- tuareg -*- *)

{shared{

open Lwt
open Eliom_content
open Html5.D
open Html5_types
open COMMON_pervasives

module EltProduct = MapProduct (struct type 'a t = 'a elt end)
open EltProduct

type ('a, 'b) c = 'a EltProduct.prod * 'b CORE_client_reaction.c

}}

{client{
  let update_elts parents to_div x =
    lwt elts = to_div x in
    EltProduct.iter2 { exec2 = fun parent elt ->
      Eliom_content.Html5.Manip.replaceAllChild parent [ elt ]
    } parents elts;
    return ()

  let react ((elts, bus) : (_, _) c) reaction =
    CORE_client_reaction.install_automatic_client_reaction bus (
      update_elts elts reaction
    )

  exception StopBackground

  let on_background elt behavior =
    Eliom_client.onload (fun () ->
      Lwt.async (fun () ->
        try_lwt
          forever (fun continue ->
            Lwt_js.sleep 1.
            >> update_elts (P1 (Only elt)) (fun () ->
              lwt y = behavior (fun () -> Lwt.fail StopBackground) in
              return (P1 (Only y))) ()
            >> continue ()
          )
        with StopBackground -> return ()))
}}

let async_elts inits computation reaction =
  let elts = EltProduct.map { fapply = Html5.Id.create_global_elt } inits in
  CORE_client_reaction.on computation (fun bus -> reaction (elts, bus)) >>
  return elts

let async_elt init computation reaction =
  lwt (P1 (Only x)) = async_elts (P1 (Only init)) computation reaction in
  return x

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
      Eliom_content.Html5.Manip.replaceChildren parent [ elt ]
    } parents elts;
    return ()

  let react ((elts, bus) : (_, _) c) reaction =
    CORE_client_reaction.react "anonymous" [bus] (
      update_elts elts reaction
    )

  exception StopBackground

  let on_background elt behavior =
    Eliom_client.onload (fun () ->
      Lwt.async (fun () ->
        try_lwt
          forever (fun continue ->
            (* FIXME: Is it the right freq? *)
            Lwt_js.sleep 1.
            >>= fun _ -> update_elts (P1 (Only elt)) (fun () ->
              lwt y = behavior (fun () -> Lwt.fail StopBackground) in
              return (P1 (Only y))) ()
            >>= continue
          )
        with StopBackground -> return ()))
}}

let async_elts inits computation reaction =
  let elts = EltProduct.map { fapply = fun x -> x } inits in
  CORE_client_reaction.on computation (fun bus -> reaction (elts, bus))
  >>= fun _ -> return elts

let async_elt init computation reaction =
  lwt (P1 (Only x)) = async_elts (P1 (Only init)) computation reaction in
  return x

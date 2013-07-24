(* -*- tuareg -*- *)

open Lwt
open HTML_app
open Eliom_content.Html5.D

let () =
  let contents () =
    if CORE_config.autotest_enabled () then
      return [ h1 [pcdata I18N.String.autotesting_title ] ]
    else
      return [ p [pcdata I18N.String.sorry_autotesting_is_disabled ] ]
  in
    Hackojo_app.register
      ~service:HTTP_services.autotest
      (fun () () ->
        lwt body = contents () in
        return (hackojo_page body)
      )

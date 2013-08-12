(* -*- tuareg -*- *)

{shared{
open Lwt
open HTML_app
open Eliom_content
open Html5
open Html5.D
open Html5_types
open CORE_autotest
open HTML_reactive
}}

let test_entry t : [< | body_content_fun] elt Lwt.t =
  let json = Json.t<string * string> in
  let run_test, launch = CORE_client_action.guard
    (fun update ->
      let t = ref 0 in
      let rec aux () =
        (incr t; Lwt_unix.sleep 1.) >>
          (update ("foo" ^ string_of_int !t, "bar");
           aux ())
      in
      aux ()
    )
  in
  lwt elt = async_div json run_test (fun c ->
    {unit {react %c (fun (description, value) ->
      return (div [
        p [pcdata description];
      ])
     )
  }})
  in
  let go = HTML_widget.button "Launch" {{ fun _ -> %launch () }} in
  let elt = div [ elt; p [ go ] ] in
  return (elt :> [ body_content_fun ] elt)

let () =
  let contents () =
    if CORE_config.autotest_enabled () then
      lwt tests = Lwt_list.map_s test_entry CORE_autotest.all in
      return (h1 [pcdata I18N.String.autotesting_title] :: tests)
    else
      return [ p [pcdata I18N.String.sorry_autotesting_is_disabled ] ]
  in
    Hackojo_app.register
      ~service:HTTP_services.autotest
      (fun () () ->
        lwt body = contents () in
        return (hackojo_page body)
      )

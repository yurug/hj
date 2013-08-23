(* -*- tuareg -*- *)

{shared{
open Lwt
open Eliom_content
open Html5
open Html5.D
open Html5_types
open HTML_app
open HTML_reactive
open CORE_autotest
open COMMON_pervasives
open EltProduct
}}

{shared{

type test_state = test_result option deriving (Json)

let string_of_test_state = function
  | Some Passed -> "OK"
  | Some Failed -> "KO"
  | None -> "..."

let show d s =
  P2 (
    span ~a:[a_class ["report"]] [pcdata d],
    span ~a:[a_class ["report"]] [pcdata (string_of_test_state s)]
  )

}}

let test_entry t =
  (** The test process transmits a report to the interface using string and a
      state represented by a [test_state]. *)
  let json = Json.t<string * test_state> in
  (** It is guarded by the client's interface. *)
  let run_test, launch = CORE_client_action.guard (fun report ->
    (** The reporting function is an asynchronous update of the user
        interface. *)
    lwt s = run t (fun s -> report (s, None)) in
    report (description t, Some s);
    return ()
  )
  in
  (** The report is viewed in the following HTML element which is updated
      asynchronously each time the test reports something. *)
  lwt (P2 (description, status)) =
    async_elts (show (description t) None) json run_test (fun c ->
      {unit {react %c (fun (description, value) ->
        return (show description value))
      }})
  in
  let td' x = td [ x ] in
  let b = HTML_widget.button I18N.String.run {{ fun _ -> %launch () }} in
  let row = tr (List.map td' [ b; description; status ]) in
  return (row, launch)

let show_tests ts =
  lwt tests = Lwt_list.map_s test_entry CORE_autotest.all in
  lwt rows = Lwt_list.map_s (fun s -> return (fst s)) tests in
  lwt launchers = Lwt_list.map_s (fun s -> return (snd s)) tests in
  let launch_all =
    {unit -> unit{ List.fold_left (fun all f -> fun () -> f (); all ()) ignore %launchers }}
  in
  let launch_all =
    HTML_widget.button I18N.String.run_all {{ fun _ -> %launch_all () }}
  in
  let td' x = td [ x ] in
  let thead =
    thead [tr (List.map td' [ launch_all; pcdata "Description"; pcdata "Status" ])]
  in
  return (tablex ~a:[a_class ["results"]] ~thead [tbody rows]
          :> [ body_content_fun ] elt)

let () =
  let contents () =
    if CORE_config.autotest_enabled () then
      lwt tests = show_tests CORE_autotest.all in
      return (h1 [pcdata I18N.String.autotesting_title] :: [ tests ])
    else
      return [ p [pcdata I18N.String.sorry_autotesting_is_disabled ] ]
  in
    Hackojo_app.register
      ~service:HTTP_services.autotest
      (fun () () ->
        lwt body = contents () in
        return (hackojo_page body)
      )

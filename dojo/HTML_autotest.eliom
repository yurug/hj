(* -*- tuareg -*- *)

{shared{
open Lwt
open Eliom_content
open Html5
open Html5.D
open Html5_types
open HTML_app
open HTML_reactive
open HTML_widget
open CORE_autotest
open COMMON_pervasives
open EltProduct
}}

{shared{

let string_of_test_result = function
  | Passed -> "OK"
  | Failed -> "KO"

let string_of_test_state = function
  | Waiting -> ""
  | Running s -> s
  | Done s -> string_of_test_result s

let show d s =
  P2 (
    span ~a:[a_class ["report"]] [pcdata d],
    span ~a:[a_class ["report"]] [pcdata (string_of_test_state s)]
  )

let test_row b d s = [
  td ~a:[a_class ["button"]] [b];
  td ~a:[a_class ["description"]] [d];
  td ~a:[a_class ["status"]] [s]
]

}}

(** [test_entry t] connects a test [t] to the user interface. *)
let test_entry t =
  (** The test process transmits a report to the interface using string and a
      state represented by a [test_state]. *)
  let json = Json.t<string * test_state> in

  (** It is guarded by the client's user interface: the [launch] function
      must be called on the client side to effectively run the test. *)
  let run_test, launch = CORE_client_action.guard (fun report ->
    (** The reporting function is an asynchronous update of the user
        interface which is passed to [CORE_autotest.run]. *)
    lwt s = run t (fun s -> report (description t, Running s)) in
    (** As soon as the test is done, we transmit the final result
        to the reporting function. *)
    return (report (description t, Done s))
  )
  in
  (** The report is viewed in the following HTML element which is
      asynchronously updated each time the test reports something. *)
  lwt (P2 (description, status)) =
    async_elts (show (description t) Waiting) json run_test (fun c ->
      {unit {react %c (fun (description, value) ->
        return (show description value))
      }})
  in
  (** Finally, we return a table row containing a button [b] to launch
      the test, a static description for the test and its current
      (dynamically updated) status. Besides, we provide the [launch]
      function as a way to run the tests by other means than the
      button [b]. *)
  let b = button (I18N.cap I18N.String.run) {{ !$ %launch }} in
  let row = tr (test_row b description status) in
  return (row, launch)

let show_tests ts =
  (** Connect the test suite to the user interface. *)
  lwt tests = Lwt_list.map_s test_entry CORE_autotest.all in

  (** Build a button to launch all the tests. *)
  lwt launchers = Lwt_list.map_s (fun s -> return (snd s)) tests in
  let run_all = {unit -> unit{ List.fold_left ( $> ) ignore %launchers }} in
  let run_all = button (I18N.cap I18N.String.run_all) {{ !$ %run_all }} in

  (** Build the HTML table for the test suite. *)
  lwt rows = Lwt_list.map_s (fun s -> return (fst s)) tests in
  let thead = thead [tr (test_row
    run_all
    (pcdata (I18N.cap I18N.String.description))
    (pcdata (I18N.cap I18N.String.status))
  )]
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

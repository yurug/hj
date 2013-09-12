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
open HTML_scroll
open CORE_document
open CORE_autotest
open CORE_client_reaction
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

let fetch_result r = wait_for r (function
  | Done s -> Some s
  | _ -> None
)

let show d s l =
  let hl = if s = Done Failed then ["highlight"] else [] in
  P3 (
    span ~a:[a_class ["report"]] [pcdata d],
    span ~a:[a_class ("report" :: hl)] [pcdata (string_of_test_state s)],
    div (List.map (fun s -> p [pcdata s]) (CORE_document.lines l))
  )

}}

(** [test_entry t] connects a test [t] to the user interface. *)
let test_entry t =
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
      asynchronously updated each time the test reports something.
      In the meantime, we log the messages inside a document that can
      be read later on using a freshly created reading service.
  *)
  let m = {test_state Lwt_mvar.t { Lwt_mvar.create_empty () }} in
  let initial_report = show (description t) Waiting CORE_document.empty_text in
  lwt ((P3 (description, status, details))) =
    async_elts initial_report run_test (fun c ->
      {reaction {HTML_reactive.react %c (
        let log = ref CORE_document.empty_text in
        fun (description, value) ->
          log := CORE_document.add_line !log (string_of_test_state value);
          Lwt_mvar.put %m value >>
          return (show description value !log))
      }})
  in
  (** Finally, we return a table row containing a button [b] to launch
      the test, a static description for the test and its current
      (dynamically updated) status. Besides, we provide the [launch]
      function as a way to run the tests by other means than the
      button [b]. *)
  let b = button [I18N.cap I18N.String.run] {{ !$ %launch }} in
  let scroll =
    HTML_scroll.hackojo_scroll
      (div [ status ])
      (div [ description ])
      ~start_shown:false
      ~description:details [b]
  in
  return (scroll, launch, {test_result Lwt.t{ fetch_result %m }})

let show_tests ts =
  (** Connect the test suite to the user interface. *)
  lwt tests = Lwt_list.map_s test_entry CORE_autotest.all in

  (** Build a button to launch all the tests. *)
  lwt launchers = Lwt_list.map_s (fun s -> return (proj_2_3 s)) tests in
  let run_all = {unit -> unit{ List.fold_left ( $> ) ignore %launchers }} in
  let run_all = button [I18N.cap I18N.String.run_all] {{ !$ %run_all }} in

  (** Build the synthetic status for the test suite. *)
  let statuses = List.map proj_3_3 tests in
  let ntests = List.length statuses in
  let show_status = div [pcdata (Printf.sprintf "[%d]" ntests)] in
  let status = Html5.Id.create_global_elt (div [show_status]) in
  let _ = {unit{on_background %status (
    let tests_status = ref %statuses in
    let oks = ref 0 and kos = ref 0 in
    let show () = div [pcdata (Printf.sprintf "%d/%d/%d" !oks !kos %ntests)] in
    fun stop ->
      (if !tests_status = [] then stop () else return ()) >>
        lwt finished, running = nchoose_split !tests_status in
        tests_status := running;
        List.iter (function
          | Passed -> incr oks
          | Failed -> incr kos
        ) finished;
        return (show ())
  )}}
  in

  (** Build the detailed report for the test suite. *)
  lwt scrolls = Lwt_list.map_s proj_1_3 tests in
  lwt ts_scroll =
    HTML_scroll.hackojo_scroll
      status
      (div [h1 [pcdata I18N.String.autotesting_title]])
      [ run_all ]
  in
  push_subscrolls scrolls ts_scroll;
  return (HTML_scroll.elt_of_hackojo_scroll ts_scroll
          :> [ body_content_fun ] elt)

let () =
  let contents () =
    if CORE_config.autotest_enabled () then
      lwt tests = show_tests CORE_autotest.all in
      return [ tests ]
    else
      return [ p [pcdata I18N.String.sorry_autotesting_is_disabled ] ]
  in
  Hackojo_app.register
    ~service:HTTP_services.autotest
    (fun () () ->
      lwt body = contents () in
      hackojo_page body
    )

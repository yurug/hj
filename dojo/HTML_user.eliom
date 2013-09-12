(* -*- tuareg -*- *)

(** This module defines the HTML home page of a user. *)

open Lwt
open Eliom_content
open Html5.D

open HTTP_services
open HTML_app
open HTML_widget
open CORE_user
open CORE_identifier
open I18N

let homepage_div id =
  logged_user () >>= (function
  | `NotLogged | `FailedLogin ->
    return (div [p [pcdata I18N.String.please_login]])
  | `Logged u ->
    return (div [])
  )

let _ =
  register_login     ~service:HTTP_services.login;
  register_logout    ~service:HTTP_services.logout;
  register_subscribe ~service:HTTP_services.subscribe

(** Subscription form. *)
let subscribe_div () =
  div ~a:[a_id "subscribe_box"] [ post_form
    ~service:HTTP_services.subscribe
    (fun (fn, (sn, (email, (login, p)))) -> [
      div ~a:[a_id "subscribe_form"] [
        field "subscribe_form_firstname" fn `Text I18N.(cap String.firstname);
        field "subscribe_form_surname" sn `Text I18N.(cap String.surname);
        field "subscribe_form_email" email `Email I18N.(cap String.email);
        field "subscribe_form_login" login `Text I18N.(cap String.username);
        field "subscribe_form_password" p `Password I18N.(cap String.password)
      ];
      string_input ~a:[a_id "subscribe_form_submit"]
        ~input_type:`Submit
        ~value:I18N.String.connect ()
    ]) () ]

let () =
  Hackojo_app.register
    ~secure_session:true
    ~service:HTTP_services.subscribe_form
    (fun () () ->
      hackojo_page [subscribe_div ()]
    )

let user_profile id =
  return (div [])

let () =
  HTML_entity.register_page_maker
    (fun id -> CORE_identifier.(is_prefix (make [label "users"]) id))
    user_profile

let user_menu u =
  let button service label x =
    a ~a:[a_class ["menu_button"]] ~service [pcdata label] x
  in
  lwt firstname = firstname u in
  return [
    button HTTP_services.root I18N.(cap String.home) ();
    button page_of firstname (identifier_to_string_list (identifier u))
  ]

(** Everybody must fill the connection box if he is not already
    connected. Once connected, it will be redirected to his
    homepage. *)
let homepage root_service =
  let connection_box ?(message="") () =
    div [
      post_form
        ~service:HTTP_services.login
        (fun (login, p) ->
          [div ~a:[a_id "connection_box"] [
            div ~a:[a_id "connection_form"] [
              field "connection_login" login `Text I18N.(cap String.username);
              field "connection_password" p `Password I18N.(cap String.password)
            ];
            div ~a:[a_id "connection_box_actions"] [
              string_input ~a:[a_id "connection_box_signin"]
                ~input_type:`Submit ~value:I18N.String.connect ();
              a HTTP_services.subscribe_form [pcdata I18N.String.subscribe ] ();
            ];
            div ~a:[a_id "connection_box_message"] [pcdata message]
          ]]) ()
    ]
  in
  logged_user () >>= (function
    | `NotLogged ->
      return (connection_box ())
    | `FailedLogin ->
      return (connection_box ~message:I18N.String.bad_login_password_pair ())
    | `Logged u ->
      lwt menu = user_menu u in
      HTML_app.set_menu menu >> homepage_div u
  )

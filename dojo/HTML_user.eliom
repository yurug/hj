(* -*- tuareg -*- *)

(** This module defines the HTML home page of a user. *)

module X = Xml
open Lwt
open Eliom_content
open Html5.D

open HTTP_services
open HTML_app
open HTML_widget
open HTML_scroll
open CORE_user
open CORE_identifier
open I18N

let notifications_scrolls u =
  let create_subscroll (kind, label) =
    let title = p [pcdata (cap label)] in
    hackojo_scroll (div []) (div [title]) []
  in
  let kinds = String.([
    `Read, many (read `Female);
    `Unread, many (not_ (read `Female))
  ]) in
  lwt subs = Lwt_list.map_s create_subscroll kinds in
  return (List.map elt_of_hackojo_scroll subs)

let exercises_scrolls u =
  (** For each kind of exercise assignment, we define a subscroll
      that itself contains one subscroll per assignment. *)
  let create_subscroll (kind, label) =
    let title = p [pcdata (cap label)] in
    hackojo_scroll (div []) (div [title]) []
  in
  let kinds = String.([ `Must, must_do; `Should, should_do; `Can, can_do ]) in
  lwt subs = Lwt_list.map_s create_subscroll kinds in
  return (List.map elt_of_hackojo_scroll subs)

let homepage u =
  let uid = CORE_user.identifier u in
  lwt title =
    lwt surname = CORE_user.surname u in
    lwt firstname = CORE_user.firstname u in
    return (h1 ~a:[a_class ["homepage_title"]] [
      pcdata (firstname ^ " " ^ surname)
    ])
  in
  lwt photo =
    let photo_filename () = "photo.jpg" in
    lwt (img_uri : string) =
      CORE_onthedisk_entity.load_source uid (photo_filename ()) >>=
        function
          | `OK s ->
            return (COMMON_file.send (
              HTML_source.source_path (module CORE_user) u (
                CORE_source.filename s
              ))
            )
          | `KO _ ->
            let u =
              "http://savepangolins.org/wp-content/uploads/2011/01/pangolin.jpg"
            in
            return (X.uri_of_string u)
    in
    return (div ~a:[a_class ["homepage_photo"]] [
      HTML_widget.fileuploader_wrapper (
        HTML_source.import (module CORE_user) u photo_filename
      ) div (img
        ~a:[a_class ["homepage_photo"]]
        ~src:(Xml.uri_of_string img_uri)
        ~alt:"Homepage photo" ());
    ])
  in
  let about =
    div [
      h2 [pcdata (I18N.String.(cap about))];
      p [pcdata "about"]
    ]
  in
  lwt notifications =
    lwt scrolls = notifications_scrolls u in
    return (div (
      h2 [pcdata (I18N.String.(cap notifications))]
      :: scrolls
    ))
  in
  lwt assignments =
    lwt scrolls = exercises_scrolls u in
    return (div (
      h2 [pcdata (I18N.String.(cap exercises))]
      :: scrolls
    ))
  in
  return (div [
    div ~a:[a_id "homepage_header"] [
      photo;
      div ~a:[a_id "about_box"] [
        title;
        about
      ];
    ];
    div ~a:[a_id "hrule"] [];
    notifications;
    assignments
  ])

let homepage_div id =
  logged_user () >>= (function
  | `NotLogged | `FailedLogin ->
    return (div [p [pcdata I18N.String.please_login]])
  | `Logged u ->
    homepage u
  )

let subscribe_then_root, subscribe_out, subscribe_result =
  HTTP_services.subscribe HTTP_services.root

let _ =
  register_login     ~service:HTTP_services.login;
  register_logout    ~service:HTTP_services.logout;
  register_subscribe subscribe_out ~service:subscribe_then_root

(** Subscription form. *)
let subscribe_div () =
  logged_user () >>= (function
    | `NotLogged | `FailedLogin ->
      lwt report_div = subscribe_result () >>= function
        | `Right _ ->
           assert false
        | `Left None ->
          return []
        | `Left (Some s) ->
          return [ div ~a:[a_id "subscribe_report"] [pcdata s] ]
      in
      let text_field id name = field id ~validator:nonempty_field name `Text in
      let mail_field id name = field id name `Email in
      let password_field id name = field id name `Password in
      return (div ~a:[a_id "subscribe_box"] (I18N.(String.([
        post_form
          ~service:subscribe_then_root
          (fun (fn, (sn, (email, (login, p)))) -> [
            div ~a:[a_id "subscribe_form"] [
              text_field "subscribe_form_firstname" fn   (cap String.firstname);
              text_field "subscribe_form_surname"   sn    (cap String.surname);
              mail_field "subscribe_form_email"     email (cap String.email);
              text_field "subscribe_form_login"     login (cap String.username);
              password_field "subscribe_form_password"  p (cap String.password)
            ];
            string_input
              ~a:[a_id "subscribe_form_submit"]
              ~input_type:`Submit
              ~value:I18N.String.connect ()
          ]) ();
      ] @ report_div))))
    | `Logged _ ->
      let root = HTTP_services.root in
      ignore {unit Lwt.t{ Eliom_client.change_page ~service:%root () () }};
      return (div [])
  )

let () =
  Hackojo_app.register
    ~secure_session:true
    ~service:HTTP_services.subscribe_form
    (fun () () ->
      lwt div = subscribe_div () in
      hackojo_page [div]
    )

let user_profile id =
  return (div [])

let () =
  HTML_entity.register_page_maker
    (fun id -> CORE_identifier.(is_prefix (make [label "users"]) id))
    user_profile

let user_menu u =
  lwt firstname = firstname u in
  return I18N.(HTTP_services.([
    menu_button page_of firstname (string_list_of_identifier (identifier u));
    menu_button logout (cap String.logout) ();
  ]))

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
      HTML_app.set_menu menu >>= fun _ -> homepage_div u
  )

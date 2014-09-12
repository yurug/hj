(* -*- tuareg -*- *)

(** This module defines the HTML home page of a user. *)

{client{
open Eliom_client
open ExtDom
}}

module X = Xml
open Lwt
open Eliom_content
open Html5.D
module H = Html5.D

open ExtPervasives
open Identifier
open ServicesHTML
open HTML
open I18N
open WidgetHTML
open UserHTTP
open StatementHTML

let homepage u =
  (* let uid = CORE_user.identifier u in *)
  (* lwt title = *)
  (*   lwt surname = CORE_user.surname u in *)
  (*   lwt firstname = CORE_user.firstname u in *)
  (*   return (h1 ~a:[a_class ["homepage_title"]] [ *)
  (*     pcdata (firstname ^ " " ^ surname) *)
  (*   ]) *)
  (* in *)
  (* lwt photo = *)
  (*   let photo_filename () = "photo.jpg" in *)
  (*   lwt (img_uri : string) = *)
  (*     CORE_onthedisk_entity.load_source uid (photo_filename ()) >>= *)
  (*       function *)
  (*         | `OK s -> *)
  (*           COMMON_file.send ( *)
  (*             HTML_source.source_path (module CORE_user) u ( *)
  (*               CORE_source.filename s *)
  (*             )) *)
  (*         | `KO _ -> *)
  (*           lwt u = *)
  (*             CORE_user.is_teacher u >>= function *)
  (*               | true -> *)
  (*                 return "http://farm4.staticflickr.com\ *)
  (*                         /3381/3661799343_e551f5a52b_z.jpg" *)
  (*               | false -> *)
  (*                 return "http://awsimx.fathermag.com\ *)
  (*                         /sports/karate_151266_XS_293x409.jpg" *)
  (*           in *)
  (*           return (X.uri_of_string u) *)
  (*   in *)
  (*   return (div ~a:[a_class ["homepage_photo"]] [ *)
  (*     (\* FIXME: 20. should be replaced by a symbolic constant. Yet, I do not *)
  (*        know how to use these symbol constants in the CSS. Should I move *)
  (*        to a programmatically defined CSS? *\) *)
  (*     HTML_widget.fileuploader_wrapper 20. 20. ( *)
  (*       HTML_source.import (module CORE_user) u photo_filename *)
  (*     ) div {{ fun () -> () }} (img *)
  (*       ~a:[a_class ["homepage_photo"]] *)
  (*       ~src:(Xml.uri_of_string img_uri) *)
  (*       ~alt:"Homepage photo" ()); *)
  (*   ]) *)
  (* in *)
  (* lwt about = *)
  (*   let get_user_infos () = *)
  (*     lwt email_value = email u in *)
  (*     lwt is_teacher_value = is_teacher u in *)
  (*     lwt last_connection_value = last_connection u in *)
  (*     return I18N.String.([ *)
  (*       [ email; email_value ]; *)
  (*       [ status; (if is_teacher_value then master else student) ]; *)
  (*       [ last_connection; last_connection_value ] *)
  (*     ]) *)
  (*   in *)
  (*   let set_user_infos = function *)
  (*     | [ _; email ] :: _ -> *)
  (*       CORE_user.set_email u email *)
  (*     | _ -> *)
  (*       (\* FIXME: This should never happen. *\) *)
  (*       return () *)
  (*   in *)
  (*   lwt editor = *)
  (*     HTML_widget.server_get_list_editor *)
  (*       ~no_header:true ~no_action:true ~no_insertion:true *)
  (*       ["Key"; "Value"] *)
  (*       get_user_infos *)
  (*       (Some set_user_infos) *)
  (*       (fun _ -> []) *)
  (*       (fun row col -> *)
  (*         match (row, col) with *)
  (*           | (0, 1) -> `RW *)
  (*           | _ -> `RO *)
  (*       ) *)
  (*   in *)
  (*   return (div [ *)
  (*     h2 [pcdata (I18N.String.(cap about))]; *)
  (*     editor *)
  (*   ]) *)
  (* in *)
  (* lwt notifications = *)
  (*   lwt scrolls = notifications_scrolls u in *)
  (*   return (div ( *)
  (*     h2 [pcdata (I18N.String.(cap notifications))] *)
  (*     :: scrolls *)
  (*   )) *)
  (* in *)
  (* lwt assignments = *)
  (*   lwt scrolls = exercises_scrolls u in *)
  (*   return (div ( *)
  (*     h2 [pcdata (I18N.String.(cap exercises))] *)
  (*     :: scrolls *)
  (*   )) *)
  (* in *)
  (* return (div [ *)
  (*   div ~a:[a_id "homepage_header"] [ *)
  (*     photo; *)
  (*     div ~a:[a_id "about_box"] [ *)
  (*       title; *)
  (*       about *)
  (*     ]; *)
  (*   ]; *)
  (*   div ~a:[a_id "hrule"] []; *)
  (*   notifications; *)
  (*   assignments *)
  (* ]) *)
  fun _ ->
    let uid = User.identifier u in
    let codes = ref [] in
    let notification_as_html id =
      let notification_box = div ~a:[a_class ["notification_box"]] in
      Notifications.(load_notification id >>= function
        | `OK n ->
          begin match n.message with
            | EphemeralMessage (_, s) | Message s ->
              return (notification_box [
                statement_as_html codes s
              ])
            | GotoExercise (id, s) ->
              return (notification_box [
                a ~service:(EntityHTML.url_of id) [
                  pcdata ("▹ " ^ string_of_identifier id)
                ] ();
                statement_as_html codes s
              ])
          end
        | `KO _ ->
          return (span []) (* FIXME *)
      )
    in
    lwt notifications =
      User.get_active_notifications uid >>= function
        | `OK ns -> Lwt_list.map_s notification_as_html ns
        | `KO _ -> return [] (* FIXME *)
    in
    let onload =
      {{
        fun _ -> WidgetHTML.highlight !(%codes)
       }}
    in
    return (div ~a:[a_onload onload] notifications)

let homepage_div id =
  logged_user () >>= (function
  | `KO _ ->
    return (fun _ -> return (div [p [pcdata I18N.String.please_login]]))
  | `OK user ->
    return (homepage user)
  )

(* let () = *)
(*   HTML_entity.register_page_maker *)
(*     (fun id -> CORE_identifier.(is_prefix (make [label "users"]) id)) *)
(*     user_profile *)

let logout =
  {{ fun _ ->
    Lwt.async (fun () ->
      %logout_server_function () >>
      change_page ~service:%root () ()
    )
  }}

let user_menu u =
  lwt firstname = User.firstname u in
  return I18N.(ServicesHTML.([
(*    menu_button page_of firstname (string_list_of_identifier (User.identifier u));*)
    menu_button_function logout (cap String.logout);
  ]))

(** Everyone must fill the connection box if she is not already
    connected. Once connected, it will be redirected to her
    homepage. *)
let homepage root_service =
  let connection_box ?(message="") () _ =

    let login_id = "connection_box_login"
    and password_id = "connection_box_password"
    in

    let login_cb =
      {{ fun _ ->
        Lwt.async (fun () ->
          let login = (ExtDom.get_input_by_id %login_id)##value
          and password = (ExtDom.get_input_by_id %password_id)##value in
          %login_server_function (Js.to_string login, Js.to_string password)
          >> change_page ~service:%root () ()
        )
       }}
    in
    return (div [
      div ~a:[a_id "connection_box"] [
        div ~a:[a_id "connection_form"] [
          div ~a:[a_id "connection_login"] [
            H.label [pcdata I18N.(cap String.username)];
            H.Raw.input ~a:[a_id login_id] ()
          ];
          div ~a:[a_id "connection_password"] [
            H.label [pcdata I18N.(cap String.password)];
            H.Raw.input ~a:[a_id password_id] ()
          ];
        ];
        div ~a:[a_id "connection_box_actions"] [
          string_input ~a:[a_id "connection_box_signin"; a_onclick login_cb]
            ~input_type:`Submit ~value:I18N.String.connect ();
        ];
        div ~a:[a_id "connection_box_message"] [pcdata message]
      ]
    ])
  in
  logged_user () >>= (function
    | `KO `FailedLogin ->
      HTML.default_menu ()
      >> return (connection_box ~message:I18N.String.bad_login_password_pair ())

    | `KO e ->
      HTML.default_menu ()
      >> return (connection_box ())

    | `OK u ->
      lwt menu = user_menu u in
      HTML.set_menu menu
      >>= fun _ -> homepage_div u
  )

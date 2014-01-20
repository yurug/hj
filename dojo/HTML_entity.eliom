(* -*- tuareg -*- *)

(** Entities HTML pages indexed by entities identifiers. *)
{shared{
open Lwt
open Eliom_content.Html5
open Eliom_content.Html5.D
open Html5_types
open Eliom_service
}}


open HTML_widget
open HTML_app
open CORE_identifier

type page_maker =
    (identifier -> bool) * (identifier -> [ body_content ] elt Lwt.t)

let page_makers : page_maker list ref =
  ref []

let register_page_maker detect retrieve =
  page_makers := (detect, retrieve) :: !page_makers

let empty_page =
  div []

let get_page id =
  try_lwt
    let (_, retrieve) =
      List.find (fun (detect, _) -> detect id) !page_makers
    in
    retrieve id
  with Not_found ->
    return empty_page

let () =
  HTML_app.Hackojo_app.register
    ~secure_session:true
    ~service:HTTP_services.page_of
    (fun id () ->
      let id = make (List.map label id) in
      lwt page = get_page (identifier_of_path id) in
      HTML_app.hackojo_page [page]
    )

let url_of id _ =
  preapply HTTP_services.page_of (string_list_of_identifier id)

let creation_page lid sid creation_service =
  let denied = div [pcdata I18N.String.access_denied] in
  CORE_user.(logged_user () >>= function
    | `Logged u -> (
      is_teacher u >>= (function
        | false -> return denied
        | true ->
          let goto_page_of _ = preapply HTTP_services.page_of lid in
          let yes_service = creation_service goto_page_of goto_page_of in
          return HTTP_services.(
            div ~a:[a_class ["message_box"]] I18N.(String.([
              p [ pcdata (does_not_exist sid) ];
              p [ pcdata do_you_want_to_create_it ];
              menu_button ~xa:[a_class ["left_side"]] yes_service (cap yes) lid;
              menu_button ~xa:[a_class ["right_side"]] root (cap no) ()
            ])))))
    | _ -> return denied)

let error_page msg =
  div ~a:[a_class ["message_box"]] I18N.String.([
    p [ pcdata msg ]
  ])

let offer_creation emake creation_service page id =
  let lid = string_list_of_identifier id in
  let sid = string_of_identifier id in
  emake id >>= function
    | `OK e ->
      page e
    | `KO (`UndefinedEntity e) ->
      creation_page lid sid creation_service
    | `KO e ->
      return (error_page (CORE_error_messages.string_of_error e))

let progress () =
  HTML_app.get_img
    ~a:[a_id "loader"; a_class ["inlined"]]
    ~alt:"loading" "ajax-loader.gif"

let get_progress = server_function Json.t<unit> (fun () -> return (progress ()))

let reactive_div es after_display get display  =
  let elt = div ~a:[a_class ["reactive"]] [progress ()] in
  lwt initial = get () in
  let update = server_function Json.t<unit> (fun () ->
    get () >>= return
  )
  in
  let e_channels = CORE_entity.(
    List.map (function (SomeEntity e) -> channel e) es
  )
  in
  let remote_get = server_function Json.t<unit> (fun () -> get ()) in
  ignore {unit{
    let process = function
      | None -> Lwt.return ()
      | Some data ->
        try_lwt
          lwt p = %get_progress () in
          return (Eliom_content.Html5.Manip.replaceAllChild %elt [p])
          >> lwt cs = %display data in
          Eliom_content.Html5.Manip.replaceAllChild %elt cs;
          Lwt.return (
            match %after_display with
              | Some f -> f ()
              | None -> ()
          )
        with e -> Lwt.return (
          Firebug.console##log (Js.string ("Exn2..." ^ Printexc.to_string e))
        )
    in
    let bench label f =
      let start = Js.to_float (jsnew Js.date_now ())##getTime () in
      let y = f () in
      let stop = Js.to_float (jsnew Js.date_now ())##getTime () in
      Firebug.console##log (Js.string (Printf.sprintf "%s in %f ms."
                                         label
                                         (stop -. start)));
      y
    in
    let config = Eliom_comet.Configuration.new_configuration () in
    Eliom_comet.Configuration.set_time_between_request config 2.;
    CORE_client_reaction.react_on_background (
      List.map Lwt_stream.clone %e_channels
    ) (
      function
        | CORE_entity.HasChanged ->
          lwt p = %get_progress () in
          return (Eliom_content.Html5.Manip.appendChild %elt p)
          (** Update the entity view. *)
          >> (try_lwt
             lwt data = bench "remote_get" (fun () -> %remote_get ()) in
             bench "process data" (fun () -> process data)
           with e -> Lwt.return (
             Firebug.console##log (Js.string ("Exn1..." ^ Printexc.to_string e)))
          ) >> return (Eliom_content.Html5.Manip.removeChild %elt p)
        | CORE_entity.MayChange ->
          Lwt.return ()
    );
    Lwt.async (fun () -> process %initial)
  }};
  return elt

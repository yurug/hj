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

{shared{
let get_progress () =
  HTML_app.get_img
    ~a:[a_id "loader"; a_class ["inlined"]]
    ~alt:"loading" "ajax-loader.gif"
}}

{client{
  let config = Eliom_comet.Configuration.new_configuration () in
  Eliom_comet.Configuration.set_always_active config
}}

let reactive_div ?condition es after_display get display  =
  let elt = div ~a:[a_class ["reactive"]] [get_progress ()] in
  let e_channels = CORE_entity.(
    List.map (function (SomeEntity e) -> channel e) es
  )
  in
  let remote_get = server_function Json.t<unit> (
    let buffer = ref None in
    fun () ->
      match !buffer with
        | None ->
          (get () >>= function
            | [] ->
              return None
            | x :: xs ->
              buffer := Some xs;
              return (Some x))
        | Some [] ->
          buffer := None;
          return None
        | Some (x :: xs) ->
          buffer := Some xs;
          return (Some x)
  )
  in
  ignore {unit{
    Lwt.async (fun () ->
    let process data =
      try_lwt
        lwt cs = %display data in
        return (List.iter (Eliom_content.Html5.Manip.appendChild %elt) cs);
      with e -> Lwt.return (
        Firebug.console##log (Js.string ("Exn2..." ^ Printexc.to_string e))
      )
    in

    let refresh () =
      try_lwt
        let p1 = get_progress () in
        let p2 = get_progress () in
        let rec flush flag =
          if flag then Eliom_content.Html5.Manip.appendChild %elt p1;
          %remote_get () >>= function
            | Some x ->
              if flag then Eliom_content.Html5.Manip.replaceAllChild %elt [p2];
              process x >> flush false
            | None ->
              (** [flag] and [None] means NoChange. *)
              if flag then
                Eliom_content.Html5.Manip.removeChild %elt p1
              else
                Eliom_content.Html5.Manip.removeChild %elt p2;
              if flag then Lwt.return () else
              Lwt.return (
                match %after_display with
                  | Some f -> f ()
                  | None -> ()
              )
        in
        flush true
      with e -> Lwt.return (
        Firebug.console##log (
          Js.string ("Exn1..." ^ Printexc.to_string e))
      )
    in
    refresh () >>
    let bench label f =
      lwt start = return (Js.to_float (jsnew Js.date_now ())##getTime ()) in
      lwt y = f () in
      return (
        let stop = Js.to_float (jsnew Js.date_now ())##getTime () in
        Firebug.console##log (Js.string (Printf.sprintf "%s in %f ms."
                                           label
                                           (stop -. start)));
        y
      )
    in
    return (CORE_client_reaction.react_on_background ?condition:%condition (
      List.map Lwt_stream.clone %e_channels
    ) (
      function
        | CORE_entity.HasChanged ->
          refresh ()
        | CORE_entity.MayChange ->
          Lwt.return ()
    ))
    )
  }};
  return elt

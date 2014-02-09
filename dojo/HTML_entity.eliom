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
  Eliom_comet.Configuration.set_always_active config true;
  Eliom_comet.Configuration.set_time_between_requests config 0.5
}}

{shared{
type 'a p =
  | NoChange
  | BOS
  | EOS
  | Data of 'a
deriving (Json)
}}

let streamed get =
  let cache = ref None in
  let buffer = ref `Closed in
  fun () ->
    try_lwt
      lwt result = match !buffer with
        | `Closed ->
          lwt data = get () in
          if !cache = Some data then
            return NoChange
          else (
            buffer := `Producing data;
            cache := Some data;
            return BOS
          )
        | `Producing [] ->
          buffer := `Closed;
          return EOS
        | `Producing (x :: xs) ->
          buffer := `Producing xs;
          return (Data x)
      in
      return result
    with e ->
      Ocsigen_messages.errlog (("Exn2..." ^ Printexc.to_string e));
      return EOS

let reactive_div
    ?condition es after_display (get : unit -> 'a list Lwt.t) display
=
  let eid = Id.new_elt_id () in
  lwt e_channels = CORE_entity.(
    Lwt_list.map_s (function (SomeEntity e) -> channel e) es
  )
  in
  let ids = CORE_entity.(
    List.map (function (SomeEntity e) -> string_of_identifier (identifier e)) es
  )
  in
  let ids = String.concat " " ids in

  (** Producer. *)
  let sget = streamed get in
  let remote_get : (unit, 'a p) server_function =
    server_function Json.t<unit> sget
  in
  let onload = {{ fun _ ->
    Lwt.async (fun () ->
      let process data =
        try_lwt
          lwt cs = %display data in
          List.iter (Eliom_content.Html5.Manip.Named.appendChild %eid) cs;
          return ()
        with e -> Lwt.return (
          Firebug.console##log (Js.string ("Exn2..." ^ Printexc.to_string e))
        )
      in

      let retry_count = ref 10 in

      let rec refresh () =
        try_lwt
          let p = get_progress () in
          let rec flush () =
            %remote_get () >>= function

              | NoChange ->
                return ()

              | BOS ->
                Eliom_content.Html5.Manip.Named.replaceAllChild %eid [p];
                flush ()

              | Data x ->
                process x >> flush ()

              | EOS ->
                Eliom_content.Html5.Manip.Named.removeChild %eid p;
                Lwt.return (
                  match %after_display with
                    | Some f -> f ()
                    | None -> ()
                )
          in
          flush ()
        with
          | e ->
            if !retry_count = 0 then (
              Firebug.console##log (
                "Sorry. Too many networking problem :" ^ Printexc.to_string e
              );
              Lwt.return ()
            )
            else (
              Firebug.console##log (
                "Retrying because of :" ^ Printexc.to_string e
              );
              decr retry_count;
              refresh ()
            )
      in

      let visible = ref false in

      let get_visible () =
        visible := true;
        refresh ()
      in

      (* FIXME: Move this to HTML_widget. *)
      let is_visible () =
        let elt = Id.get_element %eid in
        let delt = To_dom.of_element elt in
        let b = delt##getBoundingClientRect () in
        let wh = Js.Optdef.case (Dom_html.window##innerHeight)
          (fun () -> Dom_html.document##documentElement##clientHeight)
          (fun x -> x)
        in
        let ww = Js.Optdef.case (Dom_html.window##innerWidth)
          (fun () -> Dom_html.document##documentElement##clientWidth)
          (fun x -> x)
        in
           b##top    >= Js.float 0.
        && b##left   >= Js.float 0.
        && b##bottom <= Js.float (float_of_int wh)
        && b##top    <= Js.float (float_of_int ww)
      in
      let visibility_may_change = Dom.handler (fun _ ->
        if is_visible () then (
          if not !visible then Lwt.async get_visible
        )
        else (
          if !visible then visible := false
        );
        Js._true
      ) in

      Lwt.async (fun () ->
        return (List.iter (fun e -> ignore (
          Dom.addEventListener Dom_html.window (Dom.Event.make e)
            visibility_may_change Js._false
        )) [ "scroll"; "resize"; "DOMContentLoaded" ])
      );

      refresh () >> (
        (* FIXME: As long as channels are not working... *)
        let rec watch () =
          if is_visible () then refresh () else return ()
            >> Lwt_js.sleep 2.
            >> watch ()
        in
        watch ()
      ))
(*        CORE_client_reaction.react_on_background %ids %e_channels (
          function
             | CORE_entity.HasChanged ->
               Firebug.console##log (Js.string ("Has changed " ^ %ids));
               if is_visible () then refresh () else return ()
             | CORE_entity.MayChange ->
               Lwt.return ()
             ))) *)
        }}
  in
  let elt = Id.create_named_elt ~id:eid (
    div ~a:[a_class ["reactive"]; a_onload onload] [get_progress ()]
  )
  in
  return elt

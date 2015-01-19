(* -*- tuareg -*- *)

{shared{
open Lwt
open Eliom_content.Html5
open Eliom_content.Html5.D
open Html5_types
open Eliom_service
}}

open Identifier
open WidgetHTML
open TeamerHTTP
open Teamer
open ExtPervasives

let teamer_page teamer =
  let teamer_id = Teamer.identifier teamer in
  let teamer_sid = Identifier.string_of_identifier teamer_id in

  let page _  =
    let box_of_subject description =

      let SID sid = description.identifier in

      let get_slots () =
        slots_of_subject teamer description.identifier >>= function
          | `OK s -> return s
          | `KO _ -> return [] (* FIXME: Should never happen. *)
      in
      let link url =
        Raw.a ~a:[a_href (Xml.uri_of_string url)] [
          pcdata url
        ]
      in
      let slot i _ =
        let slot_idx = i in

        let view_slot () =
          lwt slots = get_slots () in
          let s = List.nth slots i in

          let uid confirmed id =
            let status = if confirmed then "(OK)" else "(?)" in
            lwt name, suid = User.(make id >>= function
              | `OK e ->
                lwt fn = fullname e in
                return (fn ^ "[" ^ string_of_identifier id ^ "]",
                        string_of_identifier id)
              | `KO _ -> return ("????", "????") (* FIXME: Should never happen. *)
            )
            in
            let unsubscribe_icon =
              WidgetHTML.small_button ["Désinscrire"] {unit -> unit{ fun () ->
                Lwt.async (fun () ->
                    %teamer_withdraw_for_user_server_function (%teamer_sid, %sid, %suid, %slot_idx)
                )
              }}
            in
            return (li [span [pcdata (name ^ status); unsubscribe_icon]])
          in
          match s with
            | Free ->
              return [span [pcdata "Libre"]]
            | Reserved (cdate, _, confirmed_uids, unconfirmed_uids) ->
              lwt cuids = Lwt_list.map_s (uid true) confirmed_uids in
              lwt uuids = Lwt_list.map_s (uid false) unconfirmed_uids in
              return [ul (cuids @ uuids)]
        in
        let status = active_div 2. view_slot in

        let self_subscribe uid =
          let suid = Identifier.string_of_identifier uid in
          let subscribe = {unit -> unit{ fun () ->
            Lwt.async (fun () ->
              %teamer_reserve_for_user_server_function (%teamer_sid, %sid, %suid, %slot_idx)
              >>
              %teamer_confirm_for_user_server_function (%teamer_sid, %sid, %suid, %slot_idx)
            )
            }}
          in
          WidgetHTML.small_button ["M'inscrire"] subscribe
        in
        lwt insert_user =
          WidgetHTML.input_button "Inscrire" {string -> bool Lwt.t{fun suid ->
            let suid = "/users/" ^ suid in (* FIXME *)
            %teamer_reserve_for_user_server_function (%teamer_sid, %sid, suid, %slot_idx)
          }}
        in
        lwt buttons =
          UserHTTP.logged_user () >>= function
            | `OK logged_user ->
              return [
                div ~a:[a_class ["teamer_buttons"]] [
                  div ~a:[a_class ["left_side"]] [self_subscribe (User.identifier logged_user)];
                  div ~a:[a_class ["right_side"]] [insert_user]
                ]
              ]
            | `KO _ ->
              return []
        in
        return (div ~a:[a_class ["teamer_team"]] (
          p [span ~a:[a_class ["bold"]] [pcdata ("Équipe " ^ string_of_int (succ i))]]
          :: status
          :: buttons
        ))
      in
      lwt slots = get_slots () in
      lwt slots = Lwt_list.mapi_s slot slots in
      return (div ~a:[a_class ["teamer_subject"]] ([
        h1 [pcdata description.title];
        p [pcdata description.description];
        p [span ~a:[a_class ["bold"]] [pcdata "Plus d'information :"]];
        link description.url
      ] @ slots)
      )
    in
    lwt subjects = subjects teamer in
    lwt boxes = Lwt_list.map_s box_of_subject subjects in
    return (div boxes)
  in
  let links _ = return (div [span [pcdata "."]]) in
  return (links, page)

let create_service user ok_page ko_page =
  Eliom_registration.Redirection.register_service
    ~path:["create_teamer"]
    ~get_params:Eliom_parameter.(suffix (list "id" (string "label")))
    (fun id () ->
      Teamer.create user (identifier_of_string_list (List.tl id)) >>= function
        | `OK e -> return (ok_page e)
          (* FIXME: Give a better error message. *)
        | `KO e -> return (ko_page "Error")
    )

let teamer_page id =
  EntityHTML.offer_creation Teamer.make create_service teamer_page id

let () =
  EntityHTML.register_page_maker
    (fun id -> Identifier.(is_prefix Teamer.path id))
    teamer_page


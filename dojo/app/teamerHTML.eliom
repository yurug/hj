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

        let view_slot =
          let prev = ref None in
          fun () ->
            lwt slots = get_slots () in
            let s = List.nth slots i in
            lwt is_complete =
              Teamer.teamer_is_open teamer >>= function
                | true ->
                  (Teamer.is_complete teamer (SID sid) slot_idx >>= function
                    | `OK `Incomplete -> return "(incomplète)"
                    | `OK `Complete -> return "(complète mais place(s) disponible(s))"
                    | `OK `Full -> return "(complète et plus de place disponible)"
                    | `KO _ -> return "(erreur à signaler à yrg@pps.univ-paris-diderot.fr)"
                  )
                | false ->
                  return "Inscriptions closes"
            in
            let uid confirmed id =
              let status =
                if confirmed then
                  span ~a:[a_class ["greenlight"]] [pcdata "OK"]
                else
                  span ~a:[a_class ["redlight"]] [pcdata "?"]
              in
              lwt name, suid = User.(make id >>= function
                | `OK e ->
                  lwt fn = fullname e in
                  return (fn, string_of_identifier id)
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
              return (
                  tr [
                    td ~a:[a_class ["table_member_name_column"]] [pcdata name];
                    td ~a:[a_class ["table_member_uid_column"]] [pcdata suid];
                    td ~a:[a_class ["table_member_status_column"]] [status];
                    td ~a:[a_class ["table_member_button_column"]] [unsubscribe_icon];
                  ]
              )
            in
            lwt status = match s with
              | Free ->
                return [p [span [pcdata ("Libre " ^ is_complete)]]]
              | Reserved (cdate, _, confirmed_uids, unconfirmed_uids) ->
                lwt cuids = Lwt_list.map_s (uid true) confirmed_uids in
                lwt uuids = Lwt_list.map_s (uid false) unconfirmed_uids in
                let items =
                  tablex ~a:[a_class ["team_table"]] [tbody (cuids @ uuids)]
                in
                return [
                  p [pcdata is_complete];
                  items
                ]
            in
            let self_subscribe uid =
              match s with
                | Free ->
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
                | _ ->
                  span []
            in
            let self_confirm uid =
              match s with
                | Reserved (_, _, _, uuids)
                    when List.exists (fun u -> Identifier.compare u uid = 0) uuids ->
                  let suid = Identifier.string_of_identifier uid in
                  let confirm = {unit -> unit{ fun () ->
                    Lwt.async (fun () ->
                      %teamer_confirm_for_user_server_function (%teamer_sid, %sid, %suid, %slot_idx)
                    )
                   }}
                  in
                  WidgetHTML.small_button ["Confirmer mon inscription"] confirm
                | _ ->
                  span []
            in
            let insert_user ?(teacher=false) uid =
              let widget () =
                  WidgetHTML.input_button "Inscrire" {string -> bool Lwt.t{fun suid ->
                    let suid = "/users/" ^ suid in (* FIXME *)
                    if %teacher then
                      %teamer_insert_user_server_function (%teamer_sid, %sid, suid, %slot_idx)
                    else
                       %teamer_reserve_for_user_server_function (%teamer_sid, %sid, suid, %slot_idx)
                  }}
              in
              match s with
                | Reserved (_, _, cuids, uuids)
                    when List.exists (fun u -> Identifier.compare u uid = 0) (uuids @ cuids) ->
                  widget ()
                | _ ->
                  if teacher then widget () else return (span [])
            in
            lwt buttons =
              teamer_is_open teamer >>= function
                | true ->
                  (UserHTTP.logged_user () >>= function
                    | `OK logged_user ->
                      lwt insert_button = insert_user (User.identifier logged_user) in
                      return [
                        div ~a:[a_class ["teamer_buttons"]] [
                          div ~a:[a_class ["left_side"]] [self_subscribe (User.identifier logged_user)];
                          div ~a:[a_class ["left_side"]] [self_confirm (User.identifier logged_user)];
                          div ~a:[a_class ["right_side"]] [insert_button]
                        ]
                      ]
                    | `KO _ ->
                      return [])
                | false ->
                  (UserHTTP.logged_user () >>= function
                    | `OK logged_user ->
                      (User.is_teacher logged_user >>= function
                        | true ->
                          lwt insert_button = insert_user ~teacher:true (User.identifier logged_user) in
                          return [
                            div ~a:[a_class ["teamer_buttons"]] [
                              div ~a:[a_class ["right_side"]] [insert_button]
                            ]
                          ]
                        | false ->
                          return [div [pcdata "Plus d'action possible."]])
                    | `KO _ ->
                      return [])
            in
            if !prev = Some s then return None else (
              prev := Some s;
              return (Some (status @ buttons))
            )
        in
        return (div ~a:[a_class ["teamer_team"]] (
          p [span ~a:[a_class ["bold"]] [pcdata ("Équipe " ^ string_of_int (succ i))]]
          :: [active_div 2. view_slot]
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

let register_reservation_direct_link () =
  HTML.Hackojo_app.register_service
    ~secure_session:true
    ~path:["direct"; "teamer_reserve"]
    ~get_params:Eliom_parameter.(suffix (all_suffix "id"))
    (fun teamer () ->
      let idx_of_cdate slots cdate =
        let idx = ref (-1) in
        List.iteri (fun i -> function Reserved (cdate', _, _, _) ->
          if cdate = cdate' then idx := i
          | _ -> ()
        ) slots;
        if !idx = -1 then return (`KO `InvalidURL) else return (`OK !idx)
      in
      let error msg =
          let links _ = return (div [span [pcdata "."]]) in
          let page _ = return (div [p [pcdata msg]]) in
          HTML.hackojo_page links page
      in
      (match List.rev teamer with
        | cdate :: sids :: revname ->
          let sid = SID sids in
          let name = identifier_of_string_list (List.rev revname) in
          let cdate = float_of_string cdate in
          Printf.eprintf "%s | %s | %f\n"
            (string_of_identifier name)
            sids
            cdate;
          (Teamer.make name >>>= fun teamer ->
           slots_of_subject teamer sid >>>= fun slots ->
           idx_of_cdate slots (Timestamp.from_float cdate) >>>= fun idx ->
           teamer_reserve_for_himself name sid idx >>>= fun () ->
           return (`OK teamer)
          )
        | _ ->
          return (`KO `InvalidURL)
      ) >>= function
        | `OK teamer ->
          HTML.default_menu ()
          >> lwt (links, page) = teamer_page teamer in
             HTML.hackojo_page links page

        | `KO `AlreadyInATeam ->
          error "Vous êtes déjà dans une équipe."

        | `KO `OnlyTheUserCanConfirm ->
          error "Seul l'utilisateur concerné peut confirmer."

        | `KO `TeamIsFull ->
          error "Plus de place dans l'équipe"

        | `KO (`UndefinedSubjectIdentifier _) ->
          error "Sujet indéfini"

        | `KO (`NotLogged | `FailedLogin) ->
          error "Vous n'êtes pas connecté."

        | `KO `InvalidURL ->
          error "L'URL n'est plus valide."

        | `KO `MustBeAlreadyInTheTeam ->
          error "L'utilisateur devrait déjà être dans l'équipe."

        | `KO `Timeout ->
          error "Délai dépassé."

        | `KO (`InternalError _ | `SystemError _ | `UndefinedEntity _ | `AlreadyExists _) ->
          error "Erreur interne."

    )

let teamer_page id =
  EntityHTML.offer_creation Teamer.make create_service teamer_page id

let _ =
  register_reservation_direct_link ()

let () =
  EntityHTML.register_page_maker
    (fun id -> Identifier.(is_prefix Teamer.path id))
    teamer_page;


open Lwt
open Entity
open InMemory
open Identifier
open ExtPervasives
open ExtProcess

let path = Identifier.from_strings [ "teamer" ]

let teamer_module = "hackojo.teamer <here@hackojo.org>"

let up () = VFS.(
  if not (exists path) then
    create teamer_module path
  else
    return (`OK ())
)

let teamer_identifier name =
  Identifier.(identifier_of_path (concat path (path_of_identifier name)))

type uid = User.identifier deriving (Json)

type slot =
  | Free
  | Reserved of Timestamp.t * Timestamp.t * uid list * uid list
deriving (Json)

type subject_identifier = SID of string
deriving (Json)

type subject_description = {
  identifier  : subject_identifier;
  title       : string;
  description : string;
  url         : string;
} deriving (Json)

type interval = {
  min : int;
  max : int
} deriving (Json)

type description = {
  subjects            : subject_description list;
  nb_team_per_subject : interval;
  nb_users_per_team   : interval;
  opening_date        : float;
  closing_date        : float;
  notifications       : float list;
} deriving (Json)

module Slots = Rb.Dict (struct
  type key = subject_identifier deriving (Json)
  type image = slot list deriving (Json)
  let compare (SID a) (SID b) = String.compare a b
end)

type description_state =
  | Nothing
  | Valid of Timestamp.t * description
  | Error of Timestamp.t * string
deriving (Json)

type internal_state = {
  description : description_state;
  slots       : Slots.t;
  checkers    : (Timestamp.t * (subject_identifier * int * string)) list;
} deriving (Json)

let subject_description_from_sid description_state sid =
  match description_state with
    | Valid (_, description) ->
      List.find (fun d -> d.identifier = sid) description.subjects
    | _ ->
      raise Not_found

let min_nb_users_per_team description_state =
  match description_state with
    | Valid (_, d) -> d.nb_users_per_team.min
    | _ -> 0

let is_open description_state =
  match description_state with
    | Valid (_, d) ->
      let now = Timestamp.(to_float (current ())) in
      let is_open = now >= d.opening_date && now <= d.closing_date in
      (* Printf.eprintf "Opening: %f Now: %f Closing: %f => %B\n%!" *)
      (*   d.opening_date now d.closing_date is_open; *)
      is_open

    | _ ->
      false

type public_change =
  | UpdateDescription
  | ReserveForUser of uid * subject_identifier * int
  | UpdateCheckers of Timestamp.t
  | Cancel of subject_identifier * int
  | Confirm of uid * subject_identifier * int
  | Withdraw of uid * subject_identifier * int

let remind_not_enough_users = ref (
  fun teamer sid title slot_idx cdate expected given limit uid -> return ()
)

let remind_confirmation_needed = ref (
  fun teamer sid slot_idx uid limit -> return ()
)

let send_cancellation_email = ref (
  fun teamer sid slot_idx uid -> return ()
)

let send_withdraw_email = ref (
  fun teamer sid slot_idx uid wuid expiration incomplete -> return ()
)

include Entity.Make (struct

  type data = internal_state deriving (Json)

  type change = public_change

  let kind = "teamer"

  exception DescriptionParsingError of string

  let act state later =
    let selfid = identifier state in
    let now = Timestamp.current () in
    let check_slot (sid, idx, limit) =
      let slots =
        try
          Slots.lookup sid (content state).slots
        with Not_found -> assert false
      in
      match List.nth slots idx with
        | Free ->
          return ()
        | Reserved (cdate, expiration, cuids, uuids) ->
          let all = cuids @ uuids in
          let nall = List.length all in
          let is_expired = Timestamp.older_than expiration now in
          let is_incomplete = nall < min_nb_users_per_team (content state).description in
          let has_unconfirmed = (uuids <> []) in
          let SID sids = sid in
          let title =
            try
              (subject_description_from_sid (content state).description sid).title
            with Not_found -> "sans titre"
          in

          (* Printf.eprintf "Expiration? %s < %s, is_incomplete:%B, is_expired: %B, has_unconfirmed:%B\n" *)
          (*   (Timestamp.to_string expiration) *)
          (*   (Timestamp.to_string now) *)
          (*   is_incomplete is_expired has_unconfirmed *)
          (* ; *)

          (if is_incomplete then
              let m = (min_nb_users_per_team (content state).description) in
              Lwt_list.iter_s
                (!remind_not_enough_users selfid sids title idx cdate m nall limit)
                all
           else
              return ()
          ) >> Lwt_list.iter_s (!remind_confirmation_needed selfid title idx limit) uuids
          >> (if (has_unconfirmed || is_incomplete) && is_expired then (
            later (Cancel (sid, idx))
          ) else return ()
          )
    in
    let check i (date, what) =
      if Timestamp.older_than date now then (
        later (UpdateCheckers now)
        >> check_slot what
      ) else return ()
    in
    Lwt_list.iteri_s check (content state).checkers

  let react state mdeps cs later =
    let update_description () =
      let description_from_json json =
        let get field json =
          let not_found () = raise (DescriptionParsingError (
            "Field " ^ field ^ " is missing in " ^ Yojson.Safe.to_string json)
          )
          in
          match json with
            | `Assoc l -> (
              try List.assoc field l with Not_found -> not_found ()
            )
            | _ -> not_found ()
        in
        let parse_list parse json =
          let not_a_list () = raise (DescriptionParsingError (
            "This is not a list " ^ Yojson.Safe.to_string json
          ))
          in
          match json with
            | `List f -> List.map parse f
            | _ -> not_a_list ()
        in
        let parse_string json =
          let not_a_string () = raise (DescriptionParsingError (
            "This is not a string " ^ Yojson.Safe.to_string json
          ))
          in
          match json with
            | `String s -> s
            | _ -> not_a_string ()
        in
        let parse_float json =
          let not_a_float () = raise (DescriptionParsingError (
            "This is not a float " ^ Yojson.Safe.to_string json
          ))
          in
          match json with
            | `Float f -> f
            | `Int x -> float_of_int x
            | _ -> not_a_float ()
        in
        let parse_float_list = parse_list parse_float in

        let parse_int json =
          let not_an_int () = raise (DescriptionParsingError (
            "This is not an integer " ^ Yojson.Safe.to_string json
          ))
          in
          match json with
            | `Int x -> x
            | _ -> not_an_int ()
        in
        let parse_subject json =
          {
            identifier = SID (parse_string (get "identifier" json));
            url = parse_string (get "url" json);
            description = parse_string (get "description" json);
            title = parse_string (get "title" json)
          }
        in
        let parse_subjects =
          parse_list parse_subject
        in
        let parse_interval json =
          let check_interval i =
            if i.min > i.max then raise (DescriptionParsingError (
              "This is not a valid interval: " ^ Yojson.Safe.to_string json
            ));
            i
          in
          check_interval {
            min = parse_int (get "min" json);
            max = parse_int (get "max" json)
          }
        in

        let parse_date json =
          let not_a_date () = raise (DescriptionParsingError (
            "This is not a date " ^ Yojson.Safe.to_string json
          ))
          in
          Unix.(match json with
            | `String s ->
              (try Scanf.sscanf s "%4d-%2d-%2d %2d:%2d:%2d" (fun year month day hour min sec ->
                fst (mktime {
                  tm_sec = sec;
                  tm_min = min;
                  tm_hour = hour;
                  tm_mday = day;
                  tm_mon = month - 1;
                  tm_year = year - 1900;
                  tm_wday = 0;
                  tm_yday = 0;
                  tm_isdst = false;
                }))
               with _ -> not_a_date ())
            | _ -> not_a_date ())
        in
        Valid (Timestamp.current (), {
          subjects = parse_subjects (get "subjects" json);
          nb_team_per_subject = parse_interval (get "nb_teams_per_subject" json);
          nb_users_per_team = parse_interval (get "nb_users_per_team" json);
          opening_date = parse_date (get "opening_date" json);
          closing_date = parse_date (get "closing_date" json);
          notifications = parse_float_list (get "notifications" json);
        })
      in

      OnDisk.load_resource (identifier state) "description.json" >>= function
        | `OK (source, _) ->
          (try_lwt
             let json = Yojson.Safe.from_string (Resource.content source) in
             return (description_from_json json)
           with Yojson.Json_error msg | DescriptionParsingError msg ->
             return (Error (Timestamp.current (), msg)))

        | `KO _ ->
          return (Error (Timestamp.current (), "No such file: description.json"))
    in

    let update_all_slots content =
      match content.description with
        | Valid (_, description) ->
          let nb_slots = description.nb_team_per_subject.max in
          let update_subject_slots slots subject =
            let sid = subject.identifier in
            let sslots =
              try
                Slots.lookup sid slots
              with Not_found -> []
            in
            let sslots =
              ExtPervasives.list_complete sslots nb_slots Free
            in
            Slots.update sid sslots slots
          in
          return {
            content with slots =
              List.fold_left update_subject_slots content.slots description.subjects
          }
        | _ -> return content
    in

    let update_slot_of_sid content sid slot_idx f =
      let slots = try Slots.lookup sid content.slots with Not_found -> assert false in
      let rcontent = ref content in
      let sslots = list_update_nth slots slot_idx (fun slot ->
        let slot, content = f slot in
        rcontent := content;
        slot
      )
      in
      { !rcontent with slots = Slots.update sid sslots content.slots }
    in

    let install_checkers content sid slot_idx now =
      match content.description with
        | Valid (_, description) ->
          let notifications = List.sort Pervasives.compare description.notifications in
          let th_expiration_time = List.(hd (rev notifications)) in
          let th_expiration = Timestamp.(to_float (shift now (th_expiration_time *. 3600.))) in
          let expiration = min th_expiration description.closing_date in
          let real_expiration_time = (expiration -. Timestamp.to_float now) /. 3600. in
          let ratio = real_expiration_time /. th_expiration_time in

          let notifications =
              List.map
                (fun hour -> Timestamp.(shift now (ratio *. hour *. 3600.)))
                notifications
          in
          let expiration = Timestamp.from_float expiration in
          let limit = Timestamp.to_string expiration in
          { content with
            checkers = List.map (fun d -> (d, (sid, slot_idx, limit))) notifications @ content.checkers
          }, expiration
          | _ -> content, Timestamp.current ()
    in
    let reserve_for_user content uid sid slot_idx =
      User.make uid >>= function
        | `KO _ ->
          return content
        | `OK _ ->
          let slots = try Slots.lookup sid content.slots with Not_found -> assert false in
          let content = ref content in
          let sslots = list_update_nth slots slot_idx (function
            | Free ->
              let now = Timestamp.current () in
              let new_content, expiration = install_checkers !content sid slot_idx now in
              content := new_content;
              Reserved (now, expiration, [], [uid])
            | Reserved (cd, expiration, cuids, uuids) ->
              Reserved (cd, expiration, cuids, uid :: uuids)
          )
          in
          let content = !content in
          return { content with slots = Slots.update sid sslots content.slots }
    in

    let update_checkers content rdate =
      return { content with checkers =
          List.filter
            (fun (date, _) ->
              Timestamp.older_than rdate date
            )
            content.checkers
      }
    in

    let cancel content sid slot_idx =
      let users = ref [] in
      let content = update_slot_of_sid content sid slot_idx (function
        | Free -> Free, content
        | Reserved (_, _, cuids, uuids) -> users := cuids @ uuids; Free, content
      )
      in
      let title =
        try
          (subject_description_from_sid content.description sid).title
        with Not_found -> "sans titre"
      in
      Lwt_list.iter_s (!send_cancellation_email (identifier state) title slot_idx) !users
      >> return content
    in

    let confirm content uid sid slot_idx =
      User.make uid >>= function
        | `KO _ ->
          return content
        | `OK _ ->
          return (update_slot_of_sid content sid slot_idx (function
            | Free -> Free, content (* FIXME: Something is probably wrong. *)
            | Reserved (cdate, expiration, cuids, uuids) ->
              let is_uid = (fun u -> Identifier.compare uid u = 0) in
              let cuids = if List.exists is_uid uuids then uid :: cuids else cuids in
              let uuids = List.filter (fun u -> not (is_uid u)) uuids in
              Reserved (cdate, expiration, cuids, uuids), content
          ))
    in

    let withdraw content uid sid slot_idx =
      return (update_slot_of_sid content sid slot_idx (function
        | Free ->
          Free, content (* FIXME: Something is probably wrong. *)
        | Reserved (cdate, expiration, cuids, uuids) ->
          let is_not_uid = (fun u -> Identifier.compare uid u <> 0) in
          let cuids = List.filter is_not_uid cuids in
          let uuids = List.filter is_not_uid uuids in
          let now = Timestamp.current () in
          if cuids = [] && uuids = [] then
            Free, content
          else (
            let content, expiration = install_checkers content sid slot_idx now in
            let title =
              try
                (subject_description_from_sid content.description sid).title
              with Not_found -> "sans titre"
            in
            let all = cuids @ uuids in
            let is_incomplete =
              List.length all < min_nb_users_per_team content.description
            in
            List.iter (fun ruid ->
              Lwt.async (fun () ->
                !send_withdraw_email (identifier state) title slot_idx ruid uid expiration is_incomplete
              )
            ) all;
            Reserved (cdate, expiration, cuids, uuids), content
          )
      ))
    in

    let push_notification () =
      let message =
        (* FIXME: Find the message in the description. *)
        Statement.(Paragraph (TCode (String (TCode ("Formation des groupes de projet pour les L2",
                                                    TNil)),
                                     TNil)))
      in
      lwt notification =
        Notifications.goto (identifier state) message
      in
      User.notify_all [] notification

    in

    let apply_change content = function
      | UpdateDescription ->
        lwt description = update_description () in
        push_notification ()
        >> update_all_slots { content with description }
      | ReserveForUser (uid, sid, slot_idx) ->
        reserve_for_user content uid sid slot_idx
      | UpdateCheckers date ->
        update_checkers content date
      | Cancel (sid, slot_idx) ->
        cancel content sid slot_idx
      | Confirm (uid, sid, slot_idx) ->
        confirm content uid sid slot_idx
      | Withdraw (uid, sid, slot_idx) ->
        withdraw content uid sid slot_idx
    in
    (* FIXME: Common pattern to be factorized out. *)
    let content0 = content state in
    lwt content = Lwt_list.fold_left_s apply_change content0 cs in
    if content == content0 then
      return NoUpdate
    else
      return (UpdateContent content)

  let current_version = "1.0"

  let converters = []

  let string_of_change = function
    | UpdateDescription ->
      "update description"
    | ReserveForUser (uid, SID id, idx) ->
      Printf.sprintf
        "Reserve slot number %d of %s for %s"
        idx id (string_of_identifier uid)
    | UpdateCheckers d ->
      "update checkers wrt " ^ Timestamp.to_string d
    | Cancel (SID id, idx) ->
      Printf.sprintf
        "Cancel reservation of slot number %d of %s"
        idx id
    | Confirm (uid, SID id, idx) ->
      Printf.sprintf
        "Confirmation for slot number %d of %s from %s"
        idx id (string_of_identifier uid)

    | Withdraw (uid, SID id, idx) ->
      Printf.sprintf
        "Withdraw for slot number %d of %s from %s"
        idx id (string_of_identifier uid)

end)

let create who name =
  let id = teamer_identifier name in
  make id >>= function
    | `OK _ -> return (`KO (`AlreadyExists (path_of_identifier id)))
    | `KO _ ->
      User.is_teacher who >>= function
        | true ->
          let data = {
            description = Nothing;
            slots = Slots.empty;
            checkers = []
          }
          in
          let init = (data, empty_dependencies, []) in
          make ~init id
        | false ->
          return (`KO `StudentsCannotCreateTeamer)

(* FIXME: This is a common pattern here: change + block observation...
   FIXME: make it a combinator. *)
let update who teamer =
  let now = Timestamp.current () in
  let uptodate t = Timestamp.older_than now t in
  change ~who:(User.identifier who) teamer UpdateDescription >>
    let rec wait () =
      observe ~who:(User.identifier who) teamer
        (fun data -> return (content data).description) >>= function
          | Valid (t, _) when uptodate t -> return (`OK ())
          | Error (t, e) when uptodate t -> return (`KO (`InvalidDescription e))
          | _ -> Lwt_unix.sleep 0.1 >> Lwt_unix.yield () >> wait ()
    in
    wait ()

let subjects teamer =
  observe teamer (fun data ->
    match (content data).description with
      | Valid (_, d) -> return d.subjects
      | _ -> return []
  )

let slots_of_subject teamer subject_identifier =
  observe teamer (fun data ->
    try_lwt
      return (`OK (Slots.lookup subject_identifier (content data).slots))
    with Not_found ->
      return (`KO (`UndefinedSubjectIdentifier subject_identifier))
  )

exception Found of subject_identifier * int

let user_in_list uid =
  List.exists (fun uid' -> Identifier.compare uid uid' = 0)

let team_of_user teamer uid =
  observe teamer (fun data ->
    try_lwt
      Slots.lwt_iter (content data).slots (fun sid slots ->
        match list_existsi slots (function
          | Reserved (_, _, cuids, uuids) -> user_in_list uid (cuids @ uuids)
          | Free -> false
        )
        with
          | None -> return ()
          | Some i -> raise_lwt (Found (sid, i)))
      >> return (`KO ())
    with Found (sid, int) ->
      return (`OK (sid, int))
  )

let number_of_users_per_team teamer =
  observe teamer (fun data ->
    match (content data).description with
      | Valid (_, description) -> return description.nb_users_per_team
      | _ -> return { min = 0; max = 0 }
  )

let max_number_of_users_per_team teamer =
  lwt i = number_of_users_per_team teamer in
  return i.max

let min_number_of_users_per_team teamer =
  lwt i = number_of_users_per_team teamer in
  return i.min

let is_complete teamer sid slot_idx =
  slots_of_subject teamer sid >>>= fun slots ->
  match List.nth slots slot_idx with
    | Free ->
      return (`OK `Incomplete)
    | Reserved (_, _, cuid, uuid) ->
      let nbu = List.length (cuid @ uuid) in
      lwt i = number_of_users_per_team teamer in
      if nbu = i.max then
        return (`OK `Full)
      else if i.min <= nbu then
        return (`OK `Complete)
      else
        return (`OK `Incomplete)

let check_reservation_is_possible teamer requesting_user sid slot_idx uid =
  lwt max_number_of_users_per_team = max_number_of_users_per_team teamer in
  let requesting_uid = User.identifier requesting_user in
  (* Check that either the slot is free or requesting_uid is in the list
     of users of the reservation. *)
  (slots_of_subject teamer sid >>>= fun slots ->
   let already_in_or_himself uids =
     if user_in_list requesting_uid (uid :: uids) then
       return (`OK ())
     else
       return (`KO `MustBeAlreadyInTheTeam)
   in
   let not_full all =
     if List.length all < max_number_of_users_per_team then
       return (`OK ())
     else
       return (`KO `TeamIsFull)
   in
   match List.nth slots slot_idx with
     | Free ->
       already_in_or_himself []
     | Reserved (_, _, cuid, uuid) ->
       let all = cuid @ uuid in
       already_in_or_himself all >>>= fun () ->
       not_full all
  )
  >>>= fun () ->
  (* Check that uid is not already in another team *)
  (team_of_user teamer uid >>= function
    | `OK _ -> return (`KO `AlreadyInATeam)
    | `KO _ -> return (`OK ()))


let wait_for_reservation_to_complete teamer uid sid slot_idx =
  let rec wait tick =
    let continue () =
      if tick > 100 then
        return (`KO `Timeout)
      else
        (Lwt_unix.sleep 0.1 >> Lwt_unix.yield () >> wait (succ tick))
    in
    team_of_user teamer uid >>= function
      | `OK (sid', slot_idx') ->
        if sid = sid' && slot_idx = slot_idx' then
          return (`OK ())
        else
          continue ()
      | `KO _ ->
        continue ()
  in
  wait 0

let reserve_for_user teamer requesting_uid sid slot_idx uid =
  check_reservation_is_possible teamer requesting_uid sid slot_idx uid >>= function
    | `OK _ ->
      change teamer (ReserveForUser (uid, sid, slot_idx))
      >> wait_for_reservation_to_complete teamer uid sid slot_idx
      >> return (`OK ())
    | `KO e ->
      return (`KO e)

let confirm teamer sid slot_idx uid =
  change teamer (Confirm (uid, sid, slot_idx))

let withdraw_for_user teamer sid slot_idx uid =
  change teamer (Withdraw (uid, sid, slot_idx))

let teamer_is_open teamer =
  observe teamer (fun data ->
    return (is_open ((content data).description))
  )

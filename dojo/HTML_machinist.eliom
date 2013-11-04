(** -*- tuareg -*- *)

{shared{
open Lwt
open Eliom_content
open Html5.D
open Html5

open HTML_widget
}}

open COMMON_pervasives
open CORE_machinist
open HTTP_services
open CORE_identifier

let selector_of_machine_kind k =
{CORE_machinist.data -> [> Html5_types.div ] elt list Lwt.t{fun _ ->
  return [div []]
}}

let edit_list label fields e get set =
  let get_editor = server_function Json.t<unit> (fun () ->
    list_editor label {
      fields;
      index_end = (fun () -> lwt l = get () in return (List.length l));
      display   = (fun k -> lwt l = get () in return (
        try
          List.nth l k
        with _ ->
          [""]
      ));

      remove    = Some (fun i _ ->
        Ocsigen_messages.errlog (Printf.sprintf "Removing %d\n" i);
        lwt l = get () in
        set (list_remove i l)
      );

      replace   = Some (fun i vs ->
        Ocsigen_messages.errlog (Printf.sprintf "Replacing %d [%s]\n" i
                                   (String.concat ", " vs));
        lwt l = get () in
        set (list_replace i vs l)
      )
    }
  )
  in
  {CORE_machinist.data -> [> Html5_types.div ] elt list Lwt.t{fun _ ->
    %get_editor () >>= fun e -> return [e]
  }}

let logins = ref [ ["bla"] ]
let get_logins () = return (!logins)
let set_logins l = return (logins := l)
let get_addresses mc = return [ ["here"] ]
let set_addresses mc = return ()
let get_sandboxes mc = return [ ["foo"] ]
let set_sandboxes mc = return ()

let machinist_page mc =
  let react_to_mc = HTML_entity.reactive_div mc in
  let get () = observe mc (fun d -> return d) in
  lwt divs = Lwt_list.map_s (react_to_mc get) [
    (selector_of_machine_kind (kind mc));
    (edit_list I18N.String.logins ["login"] mc get_logins set_logins);
    (edit_list I18N.String.addresses ["address"] mc get_addresses set_addresses);
    (edit_list I18N.String.sandboxes ["sandbox"] mc get_sandboxes set_sandboxes)
  ]
  in
  return (div divs)

let machinist_page =
  HTML_entity.offer_creation CORE_machinist.make create_service machinist_page

let () =
  HTML_entity.register_page_maker
    (fun id -> CORE_identifier.(is_prefix (make [label "machinists"]) id))
    machinist_page

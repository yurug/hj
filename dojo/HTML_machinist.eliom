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

let get_editor label fields e get set =
  let rd f = lwt l = get () in f l in
  let wr f = rd (fun l -> set (f l)) in
  let empty = List.map (fun _ -> "") fields in
  server_function Json.t<unit> (fun () ->
    List.(list_editor label {
      fields;
      index_end = (fun () -> rd (fun l -> return (length l)));
      display   = (fun k -> rd (fun l -> return (try nth l k with _ -> empty)));
      remove    = Some (fun i _ -> wr (list_remove i));
      replace   = Some (fun i vs -> wr (fun l -> list_replace i vs l))
    }))

let edit_list label fields e get set =
  let get_editor = get_editor label fields e get set in
  {CORE_machinist.data -> [> Html5_types.div ] elt list Lwt.t{fun _ ->
    lwt e = %get_editor () in
    return [e]
  }}

let machinist_page mc =
  let react_to_mc = HTML_entity.reactive_div mc in
  let get () = observe mc (fun d -> return d) in
  let logins_div =
    edit_list I18N.String.logins ["username"; "ssh key"] mc
      (fun () -> get_logins mc)
      (set_logins mc)
  in
  let addresses_div =
    edit_list I18N.String.addresses ["address"; "port"] mc
      (fun () -> get_addresses mc)
      (set_addresses mc)
  in
  lwt divs = Lwt_list.map_s (react_to_mc get) [
    (selector_of_machine_kind (kind mc));
    logins_div;
    addresses_div;
  ]
  in
  return (div divs)

let machinist_page =
  HTML_entity.offer_creation CORE_machinist.make create_service machinist_page

let () =
  HTML_entity.register_page_maker
    (fun id -> CORE_identifier.(is_prefix (make [label "machinists"]) id))
    machinist_page

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
open CORE_error_messages
open HTTP_services
open CORE_identifier

let selector_of_machine_kind k =
{CORE_machinist.data -> [> Html5_types.div ] elt list Lwt.t{fun _ ->
  return [div []]
}}

let edit_list label fields get set =
  let get_editor = get_list_editor label fields get set
    (fun i -> [])
  in
  {CORE_machinist.data -> [> Html5_types.div ] elt list Lwt.t{fun _ ->
    lwt e = %get_editor () in
    return [e]
  }}

let machinist_page mc =
  let react_to_mc = HTML_entity.reactive_div mc None in
  let get () = observe mc (fun d -> return d) in
  let logins_div =
    edit_list I18N.String.logins ["username"; "ssh key"]
      (fun () -> get_logins mc)
      (Some (set_logins mc))
  in
  let addresses_div =
    edit_list I18N.String.addresses ["address"; "port"]
      (fun () -> get_addresses mc)
      (Some (set_addresses mc))
  in
  lwt divs = Lwt_list.map_s (react_to_mc get) [
    (selector_of_machine_kind (kind mc));
    logins_div;
    addresses_div;
  ]
  in
  lwt execution_div = CORE_sandbox.(
    let d = div [] in
    lwt (_, sender) =
      CORE_client_reaction.(listening (fun c ->
        {reaction{ CORE_client_reaction.react %c (fun s ->
          return (Eliom_content.Html5.Manip.appendChild %d (p [pcdata s]))
        )}}))
    in
    let run = server_function Json.t<string> (fun cmd ->
      let observer = function
        | WriteStdout (_, l) | WriteStderr (_, l) -> return (sender l)
        | _ -> return ()
      in
      exec
        ~requirements:[Is (string_of_identifier (identifier mc))]
        ~limitations:[TimeOut 5.]
        [] cmd observer
          >>= function
            | `OK _ -> return ()
            | `KO e -> warn e; return ()
    )
    in
    let id = Id.new_elt_id () in
    let i = Id.create_named_elt id (string_input ~a:[a_onkeypress {{ fun e ->
      if e##keyCode = 13 then (* FIXME: Check the portability of this. *)
        let input_elt = Id.get_element %id in
        let input_value = (To_dom.of_input input_elt)##value in
        Lwt.async (fun () -> %run (Js.to_string input_value))
    }}] ~input_type:`Text ())
    in
    return (div [
      i; d
    ])
  )
  in
  lwt source_div = HTML_source.entity_sources_div (module CORE_machinist) mc in
  return (div (divs @ [source_div; execution_div]))

let machinist_page =
  HTML_entity.offer_creation CORE_machinist.make create_service machinist_page

let () =
  HTML_entity.register_page_maker
    (fun id -> CORE_identifier.(is_prefix (make [label "machinists"]) id))
    machinist_page

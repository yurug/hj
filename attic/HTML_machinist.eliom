(** -*- tuareg -*- *)

{shared{
open Lwt
open Eliom_content
open Html5.D
open Html5

open HTML_widget
}}

open COMMON_pervasives
open CORE_inmemory_entity
open CORE_machinist
open CORE_error_messages
open HTTP_services
open CORE_identifier

let selector_of_machine_kind k =
{CORE_machinist.data -> [> Html5_types.div ] elt list Lwt.t{fun _ ->
  return [div []]
}}

let edit_list label fields get set column_status =
  let get_editor = get_list_editor fields get set
    (fun i -> [])
    column_status
  in
  {CORE_machinist.data -> [> Html5_types.div ] elt list Lwt.t{fun _ ->
    lwt e = %get_editor () in
    return [e]
  }}

let machinist_page mc =
  let react_to_mc = HTML_entity.reactive_div [CORE_entity.SomeEntity mc] None in
  let get () = observe mc (fun d -> return [content d]) in
  let logins_div =
    edit_list I18N.String.logins ["username"; "ssh key"]
      (fun () -> get_logins mc)
      (Some (fun l -> change mc (SetLogins l)))
      (fun _ _ -> `RW)
  in
  let addresses_div =
    edit_list I18N.String.addresses ["address"; "port"]
      (fun () -> get_addresses mc)
      (Some (fun l -> change mc (SetAddresses l)))
      (fun _ _ -> `RW)
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
        {reaction{ CORE_client_reaction.react "execution_on_vm" [%c] (fun s ->
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
    let i = string_input ~input_type:`Text () in
    {unit{
      let i = To_dom.of_input %i in
      ignore (Dom_html.(addEventListener i Event.keypress (handler (fun e ->
        if e##keyCode = 13 then (* FIXME: Check the portability of this. *)
          Lwt.async (fun () -> %run (Js.to_string i##value));
        Js._true
      )) Js._true))
    }};
    return (div [
      i; d
    ])
  )
  in
  lwt source_div = HTML_source.entity_sources_div (module CORE_machinist) mc in
  return (div (divs @ [source_div; execution_div]))

let machinist_page id =
  HTML_entity.offer_creation
    CORE_machinist.make create_service machinist_page id

let () =
  HTML_entity.register_page_maker
    (fun id -> CORE_identifier.(is_prefix (make [label "machinists"]) id))
    machinist_page

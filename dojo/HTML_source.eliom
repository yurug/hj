(** -*- tuareg -*- *)

open Lwt
open Eliom_content.Html5
open Eliom_content.Html5.D
open Html5_types

open HTML_entity
open HTML_widget

let entity_sources_div
    (type d)
    (module E : CORE_entity.S with type data = d)
    (e : E.t) =
  let get_sources () =
    return (List.map (fun s -> [CORE_source.filename s]) (E.sources e))
  in
  let set_sources ss =
    (** Committing the changes to the sources. *)
    let commit = function
      | [ id ] ->
      (** If [s] does not already exist in [e], it will be
          created and assigned an empty file. *)
        let (_, get, _) = E.source id in
        lwt s = get e in
        E.update_source e id s
      | _ -> assert false
    in
    Lwt_list.iter_s commit ss
  in
  let download = {int -> unit{
    fun i ->
      Firebug.console##log ("Download")
  }}
  in
  let upload = {int -> unit{
    fun i ->
      Firebug.console##log ("Upload")
  }}
  in
  let get_editor =
    get_list_editor "Sources" ["Filenames"] get_sources (Some set_sources)
      (fun i -> [
        icon [pcdata "↑"] {{ fun _ -> %upload %i }};
        icon [pcdata "↓"] {{ fun _ -> %download %i }}
      ])
  in
  reactive_div e get_sources
    {CORE_source.filename list list -> [> Html5_types.div ] elt list Lwt.t{
      fun _ ->
        lwt e = %get_editor () in
        Lwt.return [e]
    }}

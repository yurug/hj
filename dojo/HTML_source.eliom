(** -*- tuareg -*- *)

open Lwt
open Eliom_content.Html5
open Eliom_content.Html5.D
open Html5_types

open HTML_entity
open HTML_widget

let entity_sources_div
    (type d) (module E : CORE_entity.S with type data = d) (e : E.t) =
  let get_sources () =
    return (List.map (fun s -> [CORE_source.filename s]) (E.sources e))
  in
  let set_sources s = (* FIXME *) return () in
  let get_editor =
    get_list_editor "Sources" ["Filenames"] get_sources set_sources
  in
  reactive_div e get_sources
    {CORE_source.filename list list -> [> Html5_types.div ] elt list Lwt.t{
      fun _ ->
        lwt e = %get_editor () in
        Lwt.return [e]
    }}

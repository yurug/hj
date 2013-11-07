(** -*- tuareg -*- *)

open Lwt
open Eliom_content.Html5
open Eliom_content.Html5.D
open Html5_types

open HTML_entity
open HTML_widget
open COMMON_pervasives

(* FIXME: Missing features: Download, Preview, Versioning. *)
let entity_sources_div
    (type d)
    (module E : CORE_entity.S with type data = d)
    (e : E.t) = E.(
      let get_sources () =
        return (List.map (fun s -> [CORE_source.filename s]) (sources e))
      in
      let commit id =
        (** If [s] does not already exist in [e], it will be
            created and assigned an empty file. *)
        let (_, get, _) = source id in
        (* FIXME: The following is quite inefficient... *)
        lwt s = get e in
        update_source e id s
      in
      let set_sources =
        Lwt_list.iter_s (function [ id ] -> commit id | _ -> assert false)
      in
      let download = {int -> unit{
        fun i ->
          Firebug.console##log ("Download")
      }}
      in
      let get_editor =
        (* FIXME: I8N. *)
        get_list_editor "Sources" ["Filenames"] get_sources (Some set_sources)
          (fun i ->
            let import suggested_filename = try
                return (CORE_source.filename (List.nth (sources e) i))
              with _ ->
                (** This source does not exist yet. *)
                let f = Filename.concat (vfs_directory e) suggested_filename in
                commit suggested_filename >> return f
            in
            let upload_form = fileuploader import in
            [
              icon [pcdata "↓"] {{ fun _ -> %download %i }};
              upload_form
            ])
      in
      reactive_div e get_sources
        {CORE_source.filename list list -> [> Html5_types.div ] elt list Lwt.t{
          fun _ ->
            lwt e = %get_editor () in
            Lwt.return [e]
        }}
     )

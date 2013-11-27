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
      let set_sources ss =
        Lwt_list.iter_s (function [ id ] -> commit id | _ -> assert false) ss
        >> change ~immediate:true e (fun _ -> return None)
      in
      let download = {int -> unit{
        fun i ->
          (* FIXME: to be implemented. *)
          Firebug.console##log ("Download")
      }}
      in
      let get_editor =
        (* FIXME: I8N. *)
        get_list_editor ~no_header:true "Sources" ["Filenames"] get_sources (Some set_sources)
          (fun i ->
            let import suggested_filename =
              try
                let filename = CORE_source.filename (List.nth (sources e) i) in
                return (filename, fun () -> commit filename)
              with _ ->
                (** This source does not exist yet. *)
                let f = Filename.concat (vfs_directory e) suggested_filename in
                return (f, fun () -> commit suggested_filename)
            in
            let upload_form = fileuploader import in
            [
              icon [pcdata "↓"] {{ fun _ -> %download %i }};
              upload_form
            ])
      in
      reactive_div e None get_sources
        {CORE_source.filename list list -> [> Html5_types.div ] elt list Lwt.t{
          fun _ ->
            lwt e = %get_editor () in
            Lwt.return [e]
        }}
     )

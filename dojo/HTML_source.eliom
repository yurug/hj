(** -*- tuareg -*- *)

{shared{
open Lwt
open Eliom_content.Html5
open Eliom_content.Html5.D
open Html5_types
}}

open HTML_entity
open HTML_widget
open COMMON_pervasives
open CORE_onthedisk_entity
open CORE_inmemory_entity
open CORE_error_messages

let source_path (type d) (type c)
    (module E : CORE_entity.S with type data = d and type change = c)
    (e : E.t) filename = E.(
  let path =
    CORE_identifier.(CORE_standard_identifiers.(
      string_of_path (root true (path_of_identifier (E.identifier e)))
    ))
  in
  Filename.concat path filename
)

let commit
    (type d)
    (type c)
    (module E : CORE_entity.S with type data = d and type change = c)
    (e : E.t)
    id = E.(
      (** If [s] does not already exist in [e], it will be
          created and assigned an empty file. *)
      lwt s = load_source (identifier e) id >>= function
        | `OK s -> return s
        | `KO e -> warn e; return (CORE_source.make id "")
      in
      save_source (identifier e) s >>= function
        | `OK _ -> return ()
        | `KO e -> warn e; return () (* FIXME: handle error *)
     )

let import
    (type d)
    (type c)
    (module E : CORE_entity.S with type data = d and type change = c)
    (e : E.t)
    source suggested_filename = E.(
      let eid = E.identifier e in
      let root f =
        Filename.concat
          (CORE_identifier.(CORE_standard_identifiers.(string_of_path (
            root true (path_of_identifier eid)
           )))) f
      in
      let filename = try source () with _ -> suggested_filename in
      return (root filename, fun () -> commit (module E) e filename)
    )

(* FIXME: Missing features: Preview, Versioning. *)
let entity_sources_div
    (type d)
    (type c)
    (module E : CORE_entity.S with type data = d and type change = c)
    (e : E.t) = E.(
      let get_sources () =
        lwt ss = observe e (fun d -> return (sources d)) in
        return (List.map (fun s -> [s]) ss)
      in
      let set_sources ss =
        Lwt_list.iter_s (function
          | [ id ] -> commit (module E) e id
          | _ -> assert false
        ) ss
      in
      let download =
        let link = server_function Json.t<int> (fun i ->
          lwt sources = observe e (fun d -> return (sources d)) in
          return (COMMON_file.send (
            source_path (module E) e (List.nth sources i)
          ))
        )
        in
        {int -> unit{
        fun i ->
          (* FIXME: to be implemented. *)
          Lwt.async (fun () ->
            lwt a = %link i in
            return (Dom_html.window##location##assign (Js.string a))
          )
        }}
      in
      lwt sources = observe e (fun d -> return (sources d)) in
      let get_editor =
        get_list_editor
          ~no_header:true ["Filenames"] get_sources (Some set_sources)
          (fun i ->
            let source () = List.nth sources i in
            let upload_form = fileuploader 1. 1. (import (module E) e source) in
            [
              icon [pcdata "↓"] {{ fun _ -> %download %i }};
              upload_form
            ])
          (fun _ -> `RO)
      in
      reactive_div e None get_sources
        {CORE_source.filename list list -> [> Html5_types.div ] elt list Lwt.t{
          fun _ ->
            lwt e = %get_editor () in
            Lwt.return [e]
        }}
     )

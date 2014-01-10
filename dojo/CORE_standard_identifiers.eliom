(* -*- tuareg -*- *)

(** Standard identifiers. *)

open Lwt
open CORE_identifier
open COMMON_pervasives

let ressource_path = path_of_string CORE_config.ressource_root

let root relative p =
  if relative then concat ressource_path p else p

let relativize p =
  suffix ressource_path p

let relativize_identifier p =
  identifier_of_path (relativize (path_of_identifier p))

let tests_path = from_strings [ "tests" ]
let users_path = from_strings [ "users" ]
let system_path = from_strings [ "system" ]
let exercises_path = from_strings [ "exercises" ]
let machinists_path = from_strings [ "machinists" ]

let std_paths =
  [ tests_path; users_path; system_path; machinists_path; exercises_path ]

let fresh =
  let r = ref 0 in
  fun path p ->
    incr r;
    identifier_of_path (concat path (from_strings [ p ^ string_of_int !r ]))

let assigner =
  identifier_of_path (
    concat system_path (CORE_identifier.make [ label "assigner" ])
  )

let all_identifiers_at path =
  ltry (fun lraise ->
    lwt files = COMMON_unix.ls (string_of_path (root true path)) lraise in
    return List.(
      let ids = map identifier_of_string (filter Sys.is_directory files) in
      map relativize_identifier ids
    )
  ) >>= function
    | `OK ls -> return ls
    | `KO _ -> (* FIXME: handle this correctly. *) return []

let source_filename x fname =
  if Filename.is_relative fname then
    let apath = root true (path_of_identifier x) in
    Filename.concat (string_of_path apath) fname
  else
    fname

(* -*- tuareg -*- *)

open Lwt
open Eliom_reference
open Eliom_common

let hashes =
  eref ~scope:global_scope ~persistent:"hashes_to_files" (Hashtbl.create 13)

let files =
  eref ~scope:global_scope ~persistent:"files_to_hashes" (Hashtbl.create 13)

let rec fresh_hash fname =
  lwt table = get hashes in
  let h = Random.(Printf.sprintf "%d%d%d" (bits ()) (bits ()) (bits ())) in
  if Hashtbl.mem table h then
    fresh_hash fname
  else (
    Hashtbl.add table h fname;
    set hashes table
    >> return h
  )

let hash_of_file (fname : string) =
  lwt table = get files in
  try
    return (Hashtbl.find table fname)
  with Not_found ->
    lwt h = fresh_hash fname in
    let bname = Filename.basename fname in
    Hashtbl.add table fname (h, bname);
    set files table
    >> return (h, bname)

let file_sending_service =
  Eliom_registration.File.register_service
    ~path:["sendfile"]
    ~get_params:Eliom_parameter.(suffix (string "hash" ** string "bname"))
    (fun (hash, bname) () ->
      lwt table = get hashes in
      let fname = Hashtbl.find table hash in
      Lwt.return fname
    )

let send fname =
  lwt (hash, bname) = hash_of_file fname in
  (** The following URL manual construction is probably not the
      idiomatic way of acting in an Eliom application. Yet, it
      seems to me that the service representation contains too
      much implementation details when it is transmitted in the
      client. For instance, it seems to contain the full path of
      the file on the server. I see no reason why that kind of
      information should escape the server... So I prefer a more
      low-level URL passing style here. *)
  let url =
    (* FIXME: We should move to https for security reason. *)
    (* FIXME: Yet, some browser like chrome do not accept to *)
    (* FIXME: load PDF files from untrusted https... *)
    Printf.sprintf "http://%s:%d/sendfile/%s/%s"
      (Eliom_config.get_default_hostname ())
      (Eliom_config.get_default_port ())
      hash
      bname
  in
  return (Xml.uri_of_string url)

let upload import =
  let service =
    Eliom_service.Http.post_coservice'
      ~post_params:(Eliom_parameter.(file "file"))
      ()
  in
  Eliom_registration.Action.register ~options:`NoReload ~service
    (fun () file ->
      Ocsigen_messages.errlog
        (Printf.sprintf "Received a file (%Ld):\n %s\n%s\n%s"
           file.Ocsigen_extensions.filesize
           file.Ocsigen_extensions.tmp_filename
           file.Ocsigen_extensions.raw_original_filename
           file.Ocsigen_extensions.original_basename);
        (* FIXME: Handle error. *)
      lwt dest, commit = import file.Ocsigen_extensions.original_basename in
      ExtPervasives.ltry ExtUnix.(cp file.Ocsigen_extensions.tmp_filename dest)
      >>= fun _ -> commit () >> return ()
    );
  service

(* -*- tuareg -*- *)

let send =
  let hashes = Hashtbl.create 13 in
  let rec fresh_hash () =
    let h = Printf.sprintf "%d%d%d"
      (Random.bits ()) (Random.bits ()) (Random.bits ())
    in
    if Hashtbl.mem hashes h then fresh_hash () else (
      Hashtbl.add hashes h ();
      h
    )
  in
  let h = Hashtbl.create 13 in
  fun fname ->
    try
      fst (Hashtbl.find h fname)
    with Not_found ->
      let bname = Filename.basename fname in
      let hash = fresh_hash () in
      let link =
        Eliom_registration.File.register_service
          ~path:("sendfile" :: [hash; bname])
          ~get_params:Eliom_parameter.unit
          (fun () () -> Lwt.return fname)
      in
      (** The following URL manual construction is probably not the
          idiomatic way of acting in an Eliom application. Yet, it
          seems to me that the service representation contains too
          much implementation details when it is transmitted in the
          client. For instance, it seems to contain the full path of
          the file on the server. I see no reason why that kind of
          information should escape the server... So I prefer a more
          low-level URL passing style here. *)
      let url =
        Printf.sprintf "https://%s:%d/sendfile/%s/%s"
          (Eliom_config.get_default_hostname ())
          (Eliom_config.get_default_sslport ())
          hash
          bname
      in
      Hashtbl.add h fname (url, link);
      Xml.uri_of_string url

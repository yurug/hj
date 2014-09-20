open Lwt
open ExtPervasives
open ExtProcess

let logic_shutdown () =
  User.shutdown ();
  Answers.shutdown ();
  Exercise.shutdown ();
  Notifications.shutdown ();
  Machinist.shutdown ()

let shutdown () =
  logic_shutdown ()

let facts_up path =
  let facts_dir = Filename.concat path "facts" in
  ltry (fun lraise ->
    success ~lraise (!% (Printf.sprintf "mkdir -p %s" facts_dir))
  ) >>>= fun _ ->
    Facts.set_datadir facts_dir;
    return (`OK ())

let up path =
  facts_up path
  >>>= User.up
  >>>= Exercise.up
  >>>= Machinist.up
  >>>= Notifications.up

let chroot path =
  VFS.init_root path >>>= fun () ->
  up path

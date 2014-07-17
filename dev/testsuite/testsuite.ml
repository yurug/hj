open Lwt

let ok = ref 0;;
let ko = ref 0;;

let success s =
  incr ok;
  Printf.printf "  [OK] %s\n%!" s

let failure s exn =
  incr ko;
  Printf.printf "  [KO] %s (%s)\n%!" s (Printexc.to_string exn)

let test s f =
  Lwt.catch (fun () ->
    f () >>= fun y ->
    success s;
    Lwt.return (Some y)
  ) (fun e ->
    failure s e;
    Lwt.return None
  )

let ( >=> ) c f =
  c >>= function
    | None -> return None
    | Some y -> f y

let ( !=> ) f = f ()

let scenario s f =
  let ok0 = !ok and ko0 = !ko in
  Printf.printf "* %s\n%!" s;
  Lwt_main.run (f () >>= function
    | None ->
      Printf.printf "  => Interrupted scenario (%d ok, %d ko)\n\n%!"
        (!ok - ok0)
        (!ko - ko0);
      return ()
    | Some _ ->
      Printf.printf "  => Success (%d ok, %d ko)\n\n%!"
        (!ok - ok0)
        (!ko - ko0);
      return ()
  )

let final_score () =
  Printf.printf "Total: %d ok, %d ko\n%!" !ok !ko

(* -*- tuareg -*- *)

open ExtPervasives
open Unix

let noecho_input_line () =
  let tc = tcgetattr stdin in
  tcsetattr stdin TCSANOW { tc with c_echo = false };
  let s = input_line Pervasives.stdin in
  tcsetattr stdin TCSANOW tc;
  s

let admin_password () =
  Printf.printf "password:%!";
  User.set_admin_password (noecho_input_line ())

let running () =
  Printf.printf "\nThe Hackojo is up and running!\n%!"

let _ =
  admin_password ();
  running ()

open Config
open Unix

let log msg =
  if !verbose_mode then Printf.eprintf "DEBUG: %s\n" msg

let command cmd =
  if !verbose_mode then log ("Execute `" ^ cmd ^ "'.");
  Sys.command cmd

let _ = command (Printf.sprintf "touch %s" cookie_file)

open Ocsigen_lib

let curl ?postprocess ?(forms = []) ?posts url =
  let forms =
    String.concat " "
      (List.map (fun (k, v) -> Printf.sprintf "--form %s=%s" k v) forms)
  in
  let posts = match posts with
    | Some s -> Printf.sprintf "--data \"%s\"" (Url.make_encoded_parameters s)
    | None -> ""
  in
  let cmd =
    Printf.sprintf "%s -k --cookie %s --cookie-jar %s %s %s %s %s"
      curl_executable cookie_file cookie_file posts forms url
      (match postprocess with
        | None -> ""
        | Some cmd -> "| " ^ cmd)
  in
  let status = command cmd in
  (status = 0)

let call_api service ?postprocess ?forms ?posts gets =
  curl
    (get_url () ^ "/" ^ service
     ^ (if gets = [] then "" else "?" ^ Url.make_encoded_parameters gets))
    ?postprocess
    ?forms
    ?posts

let noecho_input_line () =
  let tc = tcgetattr stdin in
  tcsetattr stdin TCSANOW { tc with c_echo = false };
  let s = input_line Pervasives.stdin in
  tcsetattr stdin TCSANOW tc;
  s

let process_options_for c options usage_msg =
  let others = ref [] in
  let anonymous x = if x = c then () else others := x :: !others in
  Arg.parse (Arg.align options) anonymous usage_msg;
  List.rev !others

let process c options usage_msg p =
  (c, fun c ->
    let others = process_options_for c options usage_msg in
    ignore (p others)
  )

let no_extra_arguments f = function
  | [] ->
    f ()
  | _ ->
    Printf.eprintf "No extra argument expected for this command.\n";
    exit 1

let run_command cs general_usage_msg =
  try
    List.assoc Sys.argv.(1) cs Sys.argv.(1)
  with _ ->
    Printf.eprintf general_usage_msg;
    exit 1

let set_opt r x = (r := Some x)

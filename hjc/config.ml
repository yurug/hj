let curl_executable = "curl"
let cookie_file     = Filename.concat (Sys.getenv "HOME") ".hjc-cookies"
let state_file      = Filename.concat (Sys.getenv "HOME") ".hjc.state"

let verbose_mode = ref false

let state : (string, string) Hashtbl.t ref = ref (Hashtbl.create 1)

let get_state =
  try
    let cin = open_in state_file in
    state := input_value cin;
    close_in cin
  with _ -> ()

let save_state () =
  try
    let cout = open_out state_file in
    output_value cout !state;
    close_out cout
  with _ -> ()

let _ = at_exit save_state

let set field value = Hashtbl.add !state field value
let get field = Hashtbl.find !state field
let get' field = try Some (get field) with _ -> None

let set_url = set "url"
let get_url () =
  try get "url"
  with Not_found ->
    Printf.eprintf "Please, login first.";
    exit 1

let set_focus = set "focus"
let get_focus () = get' "focus"

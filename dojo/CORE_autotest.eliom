(* -*- tuareg -*- *)

open Lwt

open CORE_error_messages
open COMMON_unix
open COMMON_pervasives

(** Autotesting service (disabled by default). *)

(** Test infrastructure *)

{shared{

type test_result =
  | Passed
  | Failed
deriving (Json)

type test_state =
  | Waiting
  | Running of string
  | Done of test_result
deriving (Json)

}}

type reporter = string -> unit

type test = {
  description : string;
  run         : reporter -> test_result Lwt.t
}

let make d f = {
  description = d;
  run = f;
}

let description t = t.description

let run t report = t.run report

(** Testing the server *)

let server_is_up =
  make I18N.String.the_server_is_up (fun update ->
    (** Clean up the tests folder. *)
    let path = CORE_identifier.(string_of_path (root true tests_path)) in
    ltry (
      !>> (rmdir path ~content:true)
      >>> mkdir path
    ) >>= function
      | `OK () -> return Passed
      | `KO e -> update (string_of_error e); return Failed
  )

let number_of_packets = 10
let client_server_asynchronous_communication_works =
  make I18N.String.the_asynchronous_communication_layer_is_ok
    (fun update ->
      let k = ref number_of_packets in
      let rec ping () =
        Lwt_unix.sleep 0.5 >>
          (decr k;
           if !k = 0 then
             return ()
           else
             (update (Printf.sprintf " (%02d/%02d)"
                       (number_of_packets - !k)
                       number_of_packets);
              ping ())
          )
      in
      ping ()
      >> return Passed
    )

let server_versioned_file_system_is_in_a_coherent_state =
  make I18N.String.the_vfs_is_coherent
    (fun update ->
      lwt c = CORE_vfs.check () in
      update (CORE_vfs.string_of_consistency_level c);
      return (if c = CORE_vfs.Consistent then Passed else Failed)
    )

let entity_subsystem_works =
  make I18N.String.the_entity_subsystem_works
    (fun update ->
      CORE_entity.Tests.check update >>= function
        | `OK () -> return Passed
        | `KO e  -> update (string_of_error e); return Failed
    )

let all : test list =  [
  server_is_up;
  client_server_asynchronous_communication_works;
  server_versioned_file_system_is_in_a_coherent_state;
  entity_subsystem_works
]

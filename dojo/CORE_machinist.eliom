(** -*- tuareg -*- *)

(** Machinists are sandbox providers. They allocate machines that can
    be used to perform sandboxing. These machines can be implemented
    using virtual machines, linux containers, or physical machines.

    A machinist provides only one kind of machine.

    Whatever the concrete implementation of the machine is, they
    appear as a pair [(hostname, port, login)] that is enough for us
    to execute remote SSH commands. *)

open Lwt
open Lwt_process

open CORE_entity
open CORE_identifier
open CORE_error_messages

{shared{
(* FIXME: This should not be necessary. Define proper accessors
   to public data only. This is not critical though because
   CORE_machinist is an administrator entity. *)

type address = (string * int) deriving (Json)

type machine_kind =
  | PhysicalMachines of address list
  | QemuMachine      of CORE_qemu.configuration
  | LXC              of CORE_lxc.configuration
  | Unknown
deriving (Json)

type description = {
  available_logins    : string list;
  available_addresses : address list;
  machine_kind        : machine_kind;
} deriving (Json)

}}

{client{
type data = description
}}

include CORE_entity.Make (struct
  type data = description deriving (Json)
  let react = passive
end)

type execute =
    ?timeout:float
    -> command
    -> (process_full -> unit Lwt.t)
    -> unit Lwt.t

type sandbox_interface = {
  execute : execute;
  release : unit -> unit Lwt.t
}

(** [provide_sandbox_interface mc exclusive waiting_rank] returns a
    sandbox interface. If [exclusive = true] then the underlying
    machine must not be shared between several sandboxes. In that
    case, this process can be waiting for a machine to be available,
    in which case it uses [waiting_rank] to advertise the number of
    sandboxes to wait for. *)
let provide_sandbox_interface mc exclusive waiting_rank =
  assert false

(** [capable_of mc cmd] returns [true] if [cmd] is executed
    with success by the machine of [mc]. *)
let capable_of mc cmd =
  (* FIXME *)
  true

let kind mc =
  return ()

let logins mc =
  return []

let addresses mc =
  return []

let sandboxes mc =
  return []

let make_default id =
  let default = {
    available_logins = [];
    available_addresses = [];
    machine_kind = Unknown
  }
  in
  let init = (
    default,
    CORE_inmemory_entity.empty_dependencies,
    CORE_property.empty,
    []
  )
  in
  make ~init id

let create_service ok_page ko_page =
  Eliom_registration.Redirection.register_service
    ~path:["create_machinists"]
    ~get_params:Eliom_parameter.(suffix (list "id" (string "label")))
    (fun id () ->
      try_lwt
        let id = identifier_of_string_list id in
        make_default id >>= function
          | `OK e ->
            return (ok_page e)
          | `KO e ->
            return (ko_page (string_of_error e))
      with InvalidLabel _ ->
       return (ko_page (string_of_error (`InvalidLabel (String.concat "/" id))))
    )

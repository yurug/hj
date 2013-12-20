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

open CORE_inmemory_entity
open CORE_entity
open CORE_identifier
open CORE_error_messages
open COMMON_pervasives

{shared{
(* FIXME: This should not be necessary. Define proper accessors
   to public data only. This is not critical though because
   CORE_machinist is an administrator-only entity. *)
open COMMON_waiting_list

type address = (string * int) deriving (Json)

type machine_kind =
  | PhysicalMachines of address list
  | QemuMachine      of CORE_qemu.configuration
  | LXC              of CORE_lxc.configuration
  | Unknown
deriving (Json)

type login_information = {
  username : string;
  ssh_key  : CORE_source.filename;
} deriving (Json)

type description = {
  available_logins    : login_information list;
  available_addresses : (address * login_information waiting_list) list;
  machine_kind        : machine_kind;
} deriving (Json)

}}

{client{
type data = description
}}

include CORE_entity.Make (CORE_entity.Passive (struct
  type data = description deriving (Json)
  let string_of_replacement _ = "Update."
end))

type execution_observer =
  | ObserveProcess of process_full
  | ObserveMessage of string

type execute =
    ?timeout:float
    -> string
    -> (execution_observer -> unit Lwt.t)
    -> (unit -> unit) Lwt.t

(* FIXME: Move that somewhere else. *)
let default_timeout = 1000.

let execute_using_ssh login_information addr =
  fun ?(timeout = default_timeout) cmd observer ->
    ltry (fun lraise ->
      COMMON_unix.ssh
        ~timeout
        login_information.username login_information.ssh_key
        (fst addr) (snd addr)
        cmd
        (fun p -> observer (ObserveProcess p))
        lraise
    ) >>= function
      | `OK canceler ->
        return canceler
      | `KO e ->
        (* FIXME *)
        observer (ObserveMessage "ssh error")
        >>= fun _ -> return (fun () -> ())

type sandbox_interface = {
  execute : execute;
  release : unit -> unit Lwt.t
}

let build_sandbox_interface login_information address release =
  let execute = execute_using_ssh login_information address in
  { execute; release }

let choose_waiting_list mc exclusive =
  (* FIXME: Handle exclusive = true. *)
  let smallest_waiting_list (_, wl1) (_, wl2) = COMMON_waiting_list.(
    Pervasives.compare (size wl1) (size wl2)
  )
  in
  observe mc List.(fun data ->
    match (content data).available_addresses with
      | [] -> return None
      | l -> return (Some (fst (hd (sort smallest_waiting_list l))))
  )

let wait_for_sandbox mc addr waiting_rank =
  let get_wl data =
    List.assoc addr data.available_addresses
  and set_wl data wl =
    { data with
      available_addresses = update_assoc addr wl data.available_addresses
    }
  in
  lwt ticket =
    (* FIXME: The following piece of code shows an expressiveness
       efficiency of the CORE_entity API. *)
    let tr = ref None in
    lwt data = observe mc (fun d -> return (content d)) in
    change ~immediate:true mc (UpdateContent (
      let wl = get_wl data in
      let (wl, t) = take_ticket wl in
      tr := Some t;
      set_wl data wl
    )) >>= fun _ -> (match !tr with
      | None -> (* FIXME *) assert false
      | Some t -> return t
    )
  in

  (** We react to every change of mc, looking for an update
      of the waiting list of [addr]. *)
  let rec wait last_wl =
    (* FIXME: Reimplement that by extending observation to a
       notion of "observation for a change" (with a timeout). *)
    Lwt_unix.sleep 1. >>= fun _ -> observe mc (fun state ->
      let data = content state in
      let wl = get_wl data in
      if (last_wl = wl) then
          (** No interesting change for us. *)
        wait last_wl
      else
        process wl
    )

  and process wl =
    match ticket_turn wl ticket with
      | Expired ->
        (** This ticket is expired. *)
        lwt data = observe mc (fun s -> return (content s)) in
        change ~immediate:true mc (
          UpdateContent (set_wl data (delete_ticket (get_wl data) ticket))
        ) >>= fun _ -> raise_lwt Not_found
        (* FIXME: How is that possible? *)

      | Waiting rank ->
        (** We still have to wait. Publish how long... *)
        waiting_rank rank >>= fun _ -> wait wl

      | Active r ->
        (** This is our turn. Build an interface out of this resource. *)
        let release () =
        lwt data = observe mc (fun s -> return (content s)) in
        change ~immediate:true mc (UpdateContent (
          let wl = COMMON_waiting_list.release (get_wl data) r in
          set_wl data wl
        ))
        in
        return (build_sandbox_interface (resource_value r) addr release)
  in
  lwt data = observe mc (fun data -> return (content data)) in
  process (get_wl data)


(** [provide_sandbox_interface mc exclusive waiting_rank] returns a
    sandbox interface. If [exclusive = true] then the underlying
    machine must not be shared between several sandboxes. In that
    case, this process can be waiting for a machine to be available,
    in which case it uses [waiting_rank] to advertise the number of
    sandboxes to wait for. *)
let provide_sandbox_interface mc exclusive waiting_rank =

  (** Choose a waiting list for this request. It is characterized
      by the machine address. *)
  choose_waiting_list mc exclusive >>= function
    | None ->
      raise_lwt Not_found
    | Some addr ->
      (** Wait. *)
      wait_for_sandbox mc addr waiting_rank

(** [capable_of mc cmd] returns [true] if [cmd] is executed
    with success by the machine of [mc]. *)
let capable_of mc cmd =
  (* FIXME *)
  return true

(** [lockable mc] returns [true] if [mc] is able to provide
    exclusive access to sandboxes. *)
let lockable mc =
  (* FIXME *)
  return true

let kind mc =
  return ()

let get_logins mc =
  lwt l = observe mc (fun d -> return (content d).available_logins) in
  return (List.map (fun i -> [i.username; i.ssh_key]) l)

let set_logins mc l =
  (* FIXME: each time a login is added, we must extend
     the waiting list of each address... *)
  let l =
    List.map (function
      | [username; ssh_key] -> { username; ssh_key }
      | _ -> (* FIXME: handle user error *) assert false
    ) l
  in
  lwt d = observe mc (fun d -> return (content d)) in
  change mc (UpdateContent { d with available_logins = l })

let get_addresses mc =
  lwt l = observe mc (fun d -> return (content d).available_addresses) in
  return (List.map (fun (a, _) -> [fst a; string_of_int (snd a)]) l)

let set_addresses mc l =
  let l = List.map (function
    | [hostname; port] -> (hostname, int_of_string port)
    | _ -> (* FIXME: handle user error *) assert false
  ) l
  in
  (* FIXME: For the moment, we lost all the running jobs...
     So, the current implementation only makes sense if the
     machinist is not already busy.
     We must be a bit more careful by allowing untouched
     addresses to keep their jobs. *)
  lwt d = observe mc (fun d -> return (content d)) in
  change mc (UpdateContent (
    let resource = d.available_logins in
    let l = List.map (fun a -> (a, COMMON_waiting_list.empty resource)) l in
    { d with available_addresses = l }
  ))

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

let all () =
  lwt ids = CORE_standard_identifiers.(all_identifiers_at machinists_path) in
  Lwt_list.fold_left_s (fun ls id ->
    make id >>= function `OK e -> return (e :: ls)
      | _ -> return ls
  ) [] ids

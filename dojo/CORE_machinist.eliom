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

type copy =
    ?timeout:float
    -> string list
    -> (execution_observer -> unit Lwt.t)
    -> (unit -> unit) Lwt.t

let copy_using_scp login_information addr =
  fun ?(timeout = default_timeout) srcs observer ->
    ltry (fun lraise ->
      COMMON_unix.scp
        ~timeout
        login_information.username login_information.ssh_key
        (fst addr) (snd addr)
        srcs
        (fun p -> observer (ObserveProcess p))
        lraise
    ) >>= function
      | `OK canceler ->
        return canceler
      | `KO e ->
        (* FIXME *)
        observer (ObserveMessage "scp error")
        >>= fun _ -> return (fun () -> ())

type sandbox_interface = {
  execute : execute;
  copy    : copy;
  release : unit -> unit Lwt.t
}

let build_sandbox_interface login_information address release =
  let execute = execute_using_ssh login_information address in
  let copy = copy_using_scp login_information address in
  { execute; release; copy }

type allocation_result =
  | AllocatedSandbox of sandbox_interface
  | AllocationFailed
  | InWaitingList of address * int * COMMON_waiting_list.ticket

type public_change =
  | SetLogins of string list list
  | SetAddresses of string list list
  | AllocateSandbox of (address * ticket) option * allocation_result Lwt.u
  | ReleaseSandbox of address * login_information COMMON_waiting_list.resource

include CORE_entity.Make (struct
  type data = description deriving (Json)

  type change = public_change

  let string_of_change = function
    | AllocateSandbox _ -> "Request sandbox"
    | ReleaseSandbox _ -> "Release sandbox"
    | SetLogins _ -> "Set logins"
    | SetAddresses _ -> "Set addresses"

  let react state deps cs change_later =
    (** Changes will modify waiting lists. *)
    let get_wl addr content =
      List.assoc addr content.available_addresses
    and set_wl addr wl content =
      { content with
        available_addresses = update_assoc addr wl content.available_addresses
      }
    in

    let make_change content =

      (** Sandbox allocation. *)
      let allocate_sandbox ticket waiting_thread =

        (** The following two functions communicate with the client thread
            the outcome of the allocation. *)
        let creturn = Lwt.wakeup waiting_thread in
        let no_sandbox () = creturn AllocationFailed in
        let provide s = creturn (AllocatedSandbox s) in
        let put_on_wait a r t = creturn (InWaitingList (a, r, t)) in

        (** Our strategy for scheduling is simple: we always choose the
            smallest waiting list. *)
        let choose_waiting_list exclusive =
          (* FIXME: Handle exclusive = true. *)
          let smallest_waiting_list (_, wl1) (_, wl2) = COMMON_waiting_list.(
            Pervasives.compare (size wl1) (size wl2)
          )
          in
          match content.available_addresses with
            | [] ->
              Ocsigen_messages.errlog "No addr avail.";
              return None
            | l ->
              Ocsigen_messages.errlog "Some addr avail.";
              return (Some (List.(hd (sort smallest_waiting_list l))))
        in

        (** First, take a ticket or use the one we already have. *)
        try_lwt

          lwt (content, addr, wl, ticket) =
            match ticket with
              | None ->
                begin choose_waiting_list false >>= function
                  | None ->
                    no_sandbox ();
                    raise_lwt Not_found

                  | Some (addr, wl) ->
                    let (wl, ticket) = take_ticket wl in
                    return (set_wl addr wl content, addr, wl, ticket)
                end
              | Some (addr, ticket) ->
                return (content, addr, get_wl addr content, ticket)
          in

          (** Second, act with respect to the ticket's turn. *)

          match ticket_turn wl ticket with
            | Expired ->
            (** This ticket has expired. *)
              Ocsigen_messages.errlog ("Expired ticket");
              no_sandbox ();
              return (set_wl addr (delete_ticket wl ticket) content)

            | Waiting rank ->
              Ocsigen_messages.errlog ("Waiting...");
              (** We still have to wait. Publish the rank... *)
              put_on_wait addr rank ticket;
              return content

            | Active r ->
              (** This is our turn.
                  Build an interface out of this resource. *)
              Ocsigen_messages.errlog ("Got it!");
              let release () =
                Ocsigen_messages.errlog "Release!";
                change_later (ReleaseSandbox (addr, r))
              in
              let login_info = resource_value r in
              provide (build_sandbox_interface login_info addr release);
              return content

        with Not_found ->
          return content
      in

      let release_sandbox addr r =
        let wl = get_wl addr content in
        return (set_wl addr (COMMON_waiting_list.release wl r) content)
      in

      let set_logins l =
        (* FIXME: each time a login is added, we must extend
           the waiting list of each address... *)
        let available_logins =
          List.map (function
            | [username; ssh_key] -> { username; ssh_key }
            | _ -> (* FIXME: handle user error *) assert false
          ) l
        in
        return { content with available_logins }
      in

      let set_addresses l =
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
        let resource = content.available_logins in
        let l = List.map (fun a -> (a, COMMON_waiting_list.empty resource)) l in
        return { content with available_addresses = l }
      in

      function
        | AllocateSandbox (ticket, waiting_thread) ->
          allocate_sandbox ticket waiting_thread

        | ReleaseSandbox (addr, r) ->
          release_sandbox addr r

        | SetLogins l ->
          set_logins l

        | SetAddresses l ->
          set_addresses l

    in
    let content0 = content state in
    lwt content = Lwt_list.fold_left_s make_change content0 cs in
    if content0 == content then
      return NoUpdate
    else
      return (UpdateContent content)

end)


(** [provide_sandbox_interface mc exclusive waiting_rank] returns a
    sandbox interface. If [exclusive = true] then the underlying
    machine must not be shared between several sandboxes. In that
    case, this process can be waiting for a machine to be available,
    in which case it uses [waiting_rank] to advertise the number of
    sandboxes to wait for. *)
let provide_sandbox_interface mc exclusive waiting_rank =
  (* FIXME: We should give up at some point... *)
  let rec wait ticket =
    let (waiter, wakener) = Lwt.wait () in
    change mc (AllocateSandbox (ticket, wakener))
    >> waiter >>= function
      | AllocationFailed -> raise_lwt Not_found
      | AllocatedSandbox s -> return s
      | InWaitingList (addr, rank, ticket) ->
        waiting_rank rank
        >> Lwt_unix.yield ()
        >> Lwt_unix.sleep 1. (* FIXME: This should depend on rank...*)
        >> wait (Some (addr, ticket))
  in
  wait None

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

let get_addresses mc =
  lwt l = observe mc (fun d -> return (content d).available_addresses) in
  return (List.map (fun (a, _) -> [fst a; string_of_int (snd a)]) l)

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

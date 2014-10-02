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

open InMemory
open Entity
open Identifier
open ExtPervasives

let path = Identifier.from_strings [ "machinists" ]

let machinist_module = "hackojo.machinist <here@hackojo.org>"

let up () = VFS.(
  if not (exists path) then
    create machinist_module path
  else
    return (`OK ())
)

let machinist_identifier name =
  Identifier.(identifier_of_path (concat path (path_of_identifier name)))

(* FIXME: This should not be necessary. Define proper accessors
   to public data only. This is not critical though because
   CORE_machinist is an administrator-only entity. *)
open WaitingList

type address = (string * int) deriving (Json)

type login_information = {
  username : string;
  ssh_key  : Resource.name;
} deriving (Json)

type internal_state = {
  available_logins    : login_information list;
  available_addresses : (address * login_information waiting_list) list;
} deriving (Json)

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
      ExtUnix.ssh
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
        observer (ObserveMessage "sandbox command communication error")
        >>= fun _ -> return (fun () -> ())

type copy =
    ?timeout:float
    -> clean:bool
    -> string list
    -> (execution_observer -> unit Lwt.t)
    -> (unit -> unit) Lwt.t

let copy_using_scp login_information addr =
  fun ?(timeout = default_timeout) ~clean srcs observer ->
    ltry (fun lraise ->
      (if clean then
          execute_using_ssh login_information addr "cleanup" observer
       else
          return (fun () -> ()))
      >> Lwt_unix.sleep 0.5
      >> ExtUnix.scp
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
        observer (ObserveMessage "sandbox copy communication error")
        >>= fun _ -> return (fun () -> ())

type sandbox_interface = {
  login_information : login_information;
  address           : address;
  execute           : execute;
  copy              : copy;
  release           : unit -> unit Lwt.t
}

let build_sandbox_interface login_information address release =
  let execute = execute_using_ssh login_information address in
  let copy = copy_using_scp login_information address in
  { execute; release; copy; address; login_information }

type allocation_result =
  | AllocatedSandbox of sandbox_interface
  | AllocationFailed
  | InWaitingList of address * int * WaitingList.ticket

type public_change =
  | SetLogins of (string * string) list
  | SetAddresses of (string * int) list
  | AllocateSandbox of (address * ticket) option * allocation_result Lwt.u
  | ReleaseSandbox of address * login_information WaitingList.resource

include Entity.Make (struct
  type data = internal_state deriving (Json)

  let current_version = "1.0"
  let converters = []

  let kind = "machinist"

  type change = public_change

  let string_of_change = function
    | AllocateSandbox (Some (addr, t), _) ->
      Printf.sprintf "Allocate sandbox : %s:%d [%d]"
        (fst addr)
        (snd addr)
        t
    | AllocateSandbox (None, _) ->
      Printf.sprintf "Allocate sandbox : No information"

    | ReleaseSandbox (addr, r) ->
      Printf.sprintf "Release sandbox : %s@%s:%d"
        (snd r).username
        (fst addr)
        (snd addr)
    | SetLogins _ -> "Set logins"
    | SetAddresses _ -> "Set addresses"

  let react state deps cs change_later =
    (** Changes will modify waiting lists. *)
    let get_wl addr content =
      List.assoc addr content.available_addresses
    and set_wl addr wl content =
      let content =
        { content with
          available_addresses = update_assoc addr wl content.available_addresses
        }
      in
      Log.debug (InMemory.identifier state) (
        Printf.sprintf "%s:%d %s"
          (fst addr)
          (snd addr)
          (WaitingList.string_of_wl wl));
      content
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
          let smallest_waiting_list (_, wl1) (_, wl2) = WaitingList.(
            Pervasives.compare (size wl1) (size wl2)
          )
          in
          match content.available_addresses with
            | [] ->
              return None
            | l ->
              (* FIXME: Replace this by list_min ! *)
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
              no_sandbox ();
              return (set_wl addr (delete_ticket wl ticket) content)

            | Waiting rank ->
              (** We still have to wait. Publish the rank... *)
              put_on_wait addr rank ticket;
              return content

            | Active r ->
              (** This is our turn.
                  Build an interface out of this resource. *)
              let release () =
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
        return (set_wl addr (WaitingList.release wl r) content)
      in

      let set_logins l =
        (* FIXME: each time a login is added, we must extend
           the waiting list of each address... *)
        let available_logins =
          List.map (fun (username, ssh_key) ->
            let ssh_key =
              OnDisk.resource_real_path (InMemory.identifier state) ssh_key
            in
            { username; ssh_key }
          ) l
        in
        return { content with available_logins }
      in
      let set_available_addresses content l =
        (* FIXME: For the moment, we lost all the running jobs...
           So, the current implementation only makes sense if the
           machinist is not already busy.
           We must be a bit more careful by allowing untouched
           addresses to keep their jobs. *)
        let resource = content.available_logins in
        let l = List.map (fun a -> (a, WaitingList.empty resource)) l in
        return { content with available_addresses = l }
      in

      function
        | AllocateSandbox (ticket, waiting_thread) ->
          allocate_sandbox ticket waiting_thread

        | ReleaseSandbox (addr, r) ->
          release_sandbox addr r

        | SetLogins l ->
          lwt content = set_logins l in
          (** The login information inside available_addresses
              must be refreshed. *)
          let addresses = fst (List.split content.available_addresses) in
          set_available_addresses content addresses

        | SetAddresses l ->
          set_available_addresses content l

    in
    let refresh_wl content (addr, wl) =
      let wl' = WaitingList.fresh_for_this_session wl in
      if wl == wl' then content else set_wl addr wl' content
    in
    let content0 = content state in
    let content =
      List.fold_left refresh_wl content0 content0.available_addresses
    in
    lwt content = Lwt_list.fold_left_s make_change content cs in
    if content0 == content then
      return NoUpdate
    else
      return (UpdateContent content)

end)


(** [provide_sandbox_interface mc exclusive waiting_rank] returns a
    sandbox interface. If [exclusive = true] then the underlying
    machine must not be shared between several sandboxes. In that
    case, this process can be waiting for a machine to be available,
    then it uses [waiting_rank] to advertise the number of sandboxes
    to wait for. *)
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
        >> Lwt_unix.sleep 0.1 (* FIXME: This should depend on rank...*)
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

let set_logins mc logins =
  change mc (SetLogins logins)

let get_logins mc =
  lwt l = observe mc (fun d -> return (content d).available_logins) in
  return (List.map (fun i -> [i.username; i.ssh_key]) l)

let get_addresses mc =
  lwt l = observe mc (fun d -> return (content d).available_addresses) in
  return (List.map (fun (a, _) -> [fst a; string_of_int (snd a)]) l)

let set_addresses mc addrs =
  change mc (SetAddresses addrs)

let create id =
  let id = machinist_identifier id in
  let default = {
    available_logins = [];
    available_addresses = [];
  }
  in
  let init = (
    default,
    InMemory.empty_dependencies,
    []
  )
  in
  make ~init id

(* FIXME: This should be moved elsewhere. *)
let all_identifiers_at path =
  ltry (fun lraise ->
    lwt files = ExtUnix.ls (VFS.real_path path) lraise in
    return List.(
      let ids = map identifier_of_string (filter Sys.is_directory files) in
      map VFS.relativize_identifier ids
    )
  ) >>= function
    | `OK ls -> return ls
    | `KO _ -> (* FIXME: handle this correctly. *) return []

let all () =
  lwt ids = all_identifiers_at path in
  Lwt_list.fold_left_s (fun ls id ->
    make id >>= function `OK e -> return (e :: ls)
      | _ -> return ls
  ) [] ids

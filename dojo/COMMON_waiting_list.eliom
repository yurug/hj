(** -*- tuareg -*- *)

(** This module provides waiting lists, useful for
    fine-tuned scheduling. *)

open COMMON_pervasives

let this_session_timestamp = Unix.gettimeofday ()

{shared{

type ticket = int deriving (Json)

type slot = ticket option ref deriving (Json)

type 'a resource = slot * 'a deriving (Json)

type 'a t = {
  timestamp : float;
  capacity : 'a resource list;
  tickets  : ticket list
} deriving (Json)

type 'a waiting_list = 'a t deriving (Json)

}}

let string_of_ticket t = string_of_int t

let string_of_wl wl =
  Printf.sprintf "[%d] {%s}"
    (List.length wl.capacity)
    (String.concat "; " (List.map string_of_ticket wl.tickets))

let empty resource =
  let capacity = List.map (fun a -> (ref None, a)) resource in
  {
    timestamp = this_session_timestamp;
    capacity;
    tickets = []
  }

let fresh_for_this_session wl =
  if wl.timestamp = this_session_timestamp then
    wl
  else
    empty (List.map snd wl.capacity)

let size wl = List.length wl.tickets

let capacity wl = List.length wl.capacity

let full wl = size wl >= capacity wl

(** Invariant 1: the first [n] tickets are assigned to the [n]
    first cells of [capacity]. *)
let invariant_first_tickets_are_assigned wl =
  let assigned_ticket (t : ticket) =
    List.exists (fun (s, _) -> !s = Some t) wl.capacity
  in
  let tickets = list_cut (capacity wl) wl.tickets in
  List.for_all assigned_ticket tickets

let take_ticket q =
  let q = fresh_for_this_session q in
  let rec aux () =
    let t = Random.bits () in
    if List.mem t q.tickets then aux () else t
  in let t = aux () in
  ({ q with tickets = q.tickets @ [t] }, t)

let delete_ticket wl t =
  (* FIXME: Should we be able to delete a ticket that is
     actually being processed? *)
  List.iter (fun (s, _) -> if !s = Some t then s := None) wl.capacity;
  { wl with tickets = List.filter (( <> ) t) wl.tickets }

let empty_slot s = (!s = None)

let allocate_resource wl t =
  try
    let r = List.find (fun r -> empty_slot (fst r)) wl.capacity in
    fst r := Some t;
    r
  with Not_found ->
    (** By precondition and invariant 1 of wl. *)
    assert false

exception InvalidUseOfResource

type 'a ticket_status =
  | Active  of 'a resource
  | Waiting of int
  | Expired

(** [t] must be a ticket of [wl] that has never quit. *)
let ticket_turn wl t =
  try
    let idx = list_index_of t wl.tickets in
    if idx < capacity wl then
      Active (allocate_resource wl t)
    else
      Waiting (capacity wl - idx + 1)
  with Not_found ->
    Expired

let release q r =
  match !(fst r) with
    | None ->
      raise InvalidUseOfResource
    | Some t ->
      (fst r) := None;
      { q with tickets = List.filter (( <> ) t) q.tickets }

let resource_value r = snd r

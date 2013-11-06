(** -*- tuareg -*- *)

(** This module provides waiting lists, useful for
    fine-tuned scheduling. *)

{shared{

open COMMON_pervasives

type ticket = int deriving (Json)

type slot = ticket option ref deriving (Json)

type 'a resource = slot * 'a deriving (Json)

type 'a t = {
  capacity : 'a resource list;
  tickets  : ticket list
} deriving (Json)

type 'a waiting_list = 'a t deriving (Json)


let empty resource =
  let capacity = List.map (fun a -> (ref None, a)) resource in
  {
    capacity;
    tickets = []
  }

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
  let rec aux () =
    let t = Random.bits () in
    if List.mem t q.tickets then aux () else t
  in let t = aux () in
  ({ q with tickets = q.tickets @ [t] }, t)

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

(** [t] must be a ticket of [wl] that has never quit. *)
let ticket_turn wl t =
  try
    let idx = list_index_of t wl.tickets in
    if idx < capacity wl then
      Active (allocate_resource wl t)
    else
      Waiting (capacity wl - idx + 1)
  with Not_found ->
    (** By precondition. *)
    assert false

let release q r =
  match !(fst r) with
    | None ->
      raise InvalidUseOfResource
    | Some t ->
      (fst r) := None;
      { q with tickets = list_remove t q.tickets }

let resource_value r = snd r

}}

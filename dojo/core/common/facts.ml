(* -*- tuareg -*- *)
(** This module implements a base of facts. *)

open Dstream
open Identifier
open Timestamp

(* FIXME: Make sure this identifier is reserved. *)
(** The following identifier represents an abstract entity which
    has complete visibility over the facts. *)
let daemon = identifier_of_string_list ["__hj__"; "daemon"]

type 'a predicate = {
  name        : string;
  ty          : 'a ty;
  visibility  : identifier filter;
  permission  : identifier filter;
}

and statement =
    Statement : timestamp * identifier * 'a predicate * 'a -> statement

and 'a request =
  | Is : 'b predicate * ('a, 'b) pattern -> 'a request

and 'a filter =
  | True
  | Satisfy of 'a request

and ('a, 'b) pattern =
  | Hole : ('a, 'a) pattern
  | Fst  : 'c * ('a, 'b) pattern -> ('a, 'c * 'b) pattern
  | Snd  : ('a, 'b) pattern * 'c -> ('a, 'b * 'c) pattern

and 'a ty =
  | TInt : int ty
  | TString : string ty
  | TFloat : float ty
  | TIdentifier : identifier ty
  | TTimestamp : timestamp ty
  | TStatement : statement_idx ty
  | TPair : 'a ty * 'b ty -> ('a * 'b) ty

and statement_idx = StatementIdx of int * int

type ('a, 'b) eqprop =
  | Eq  : ('a, 'a) eqprop
  | NEq : ('a, 'b) eqprop

let rec coerce : type a b. a ty -> b ty -> (a, b) eqprop = fun ty ty' ->
  match (ty, ty') with
    | TInt, TInt -> Eq
    | TString, TString -> Eq
    | TFloat, TFloat -> Eq
    | TIdentifier, TIdentifier -> Eq
    | TStatement, TStatement -> Eq
    | TTimestamp, TTimestamp -> Eq
    | TPair (a, b), TPair (a', b') ->
      begin match coerce a a', coerce b b' with
        | Eq, Eq -> Eq
        | _, _ -> NEq
      end
    | _, _ -> NEq

let make_predicate name visibility permission ty =
  { name; ty; visibility; permission }

let timestamp_of_statement (Statement (t, _, _, _)) = t

module M = ArraySimpleMap.Make (struct
  type data = statement
  type key = timestamp
  let get_key = timestamp_of_statement
  let compare = Timestamp.compare
end)

type chunk = M.t

type serialized_chunk = {
  mutable loaded     : bool;
          idx        : int;
  mutable data       : chunk;
}

type statements = serialized_chunk list

let statements_chunks_counter = ref 0

let statements_chunks : statements ref = ref []

let statements_chunks_size = 1024 * 1024

let datadir = ref None

exception UndefinedFactsDataDirectory

let get_datadir () = match !datadir with
  | None -> Error.fatal UndefinedFactsDataDirectory
  | Some d -> d

let set_datadir s = datadir := Some s

let filename_of_chunk_idx idx =
  Filename.concat (get_datadir ()) ("facts." ^ string_of_int idx)

let save_chunk c =
  if c.loaded then (
    let cout = open_out_bin (filename_of_chunk_idx c.idx) in
    Marshal.to_channel cout c [];
    close_out cout
  )

let save () =
  List.iter save_chunk !statements_chunks

let unload c =
  c.loaded <- false;
  c.data <- M.make 0

let gc keep =
  let rec aux k = function
    | [] -> ()
    | c :: cs -> if k <= 0 then unload c; aux (k - 1) cs
  in
  aux keep !statements_chunks

exception StatementsChunksAllocationFailed

let new_statements_chunks () =
  try
    let chunk = M.make statements_chunks_size in
    let serialized_chunk = {
      loaded     = true;
      idx        = !statements_chunks_counter;
      data       = chunk;
    }
    in
    incr statements_chunks_counter;
    statements_chunks := serialized_chunk :: !statements_chunks
  with _ -> raise StatementsChunksAllocationFailed

let load_data idx =
  let fname = filename_of_chunk_idx idx in
  let cin = open_in_bin fname in
  let chunk = (Marshal.from_channel cin : chunk) in
  close_in cin;
  chunk

let load_chunk c =
  c.data <- load_data c.idx;
  c.loaded <- true

let ensure_chunk_is_loaded c =
  if not c.loaded then load_chunk c;
  c

let rec latest () =
  match !statements_chunks with
    | [] -> new_statements_chunks (); latest ()
    | c :: _ -> ensure_chunk_is_loaded c

type iterator = Iterator of chunk * serialized_chunk list * M.iterator

let close_iterator (Iterator (chunk, _, idx)) : iterator =
  Iterator (chunk, [], idx)

let latest_statement_iterator () : iterator =
  match !statements_chunks with
    | [] -> assert false
    | x :: xs -> Iterator (x.data, xs, M.start x.data)

let next_older (Iterator (chunk, cs, idx)) =
  if M.at_the_end idx then (
    match cs with
      | [] -> Iterator (chunk, [], idx)
      | chunk :: cs -> Iterator (chunk.data, cs, M.start chunk.data)
  ) else Iterator (chunk, cs, M.next idx)

let at_the_end (Iterator (_, cs, _)) = (cs = [])

let get (Iterator (chunk, _, idx)) = M.value chunk idx

let rec push s =
  try
    let chunk = latest () in
    (chunk.idx, M.insert chunk.data s)
  with M.Full ->
    new_statements_chunks ();
    push s

let state who pred x =
  push (Statement (Timestamp.current (), who, pred, x))

let iter f =
  List.iter (fun c ->
    let was_loaded = c.loaded in
    M.rev_iter (ensure_chunk_is_loaded c).data f;
    if not was_loaded then unload c
  ) !statements_chunks

let rec satisfy
: type a. identifier -> a request -> statement -> a option =
fun asked_by r (Statement (t, who, p, x)) ->
  if not (is_visible p asked_by) then None
  else match r with
    | Is (p', pat) when is_visible p' asked_by ->
      begin match coerce p.ty p'.ty with
        | Eq -> match_pattern pat x
        | NEq -> None
      end
    | _ ->
      None

and match_pattern
: type a b. (a, b) pattern -> b -> a option = fun p x ->
  match p, x with
    | Hole, x ->
      Some x
    | Fst (fp, p), (fx, x) when fp = fx ->
      match_pattern p x
    | Snd (p, sp), (x, sx) when sp = sx ->
      match_pattern p x
    | _, _ ->
      None

and is_visible
: type a. a predicate -> identifier -> bool = fun p id ->
  id = daemon || filter p.visibility (Identifier.equal id)

and filter
: type a. a filter -> (a -> bool) -> bool = fun f first ->
  match f with
  | True -> true
  | Satisfy r -> not (is_empty (search ~first daemon always r))

and search
: type a. ?first:(a -> bool) -> identifier -> interval -> a request -> a stream
= fun ?first asked_by interval r ->
   from_fun (latest_statement_iterator ()) (fun idx ->
     if at_the_end idx then Done
     else
       let statement = get idx in
       let ts = timestamp_of_statement statement in
       if not (Timestamp.mem ts interval) then
         Done
       else match satisfy asked_by r statement with
         | None -> Skip (next_older idx)
         | Some x ->
           match first with
             | Some pred when pred x -> Yield (x, close_iterator idx)
             | _ -> Yield (x, next_older idx)

   )

let ask asked_by interval r =
  search asked_by interval r

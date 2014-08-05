(* -*- tuareg -*- *)
(** This module implements a base of facts. *)

open Identifier
open Timestamp

(* FIXME: Make sure this identifier is reserved. *)
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

let make_predicate name visibility permission ty =
  { name; ty; visibility; permission }

module M = ArraySimpleMap.Make (struct
  type data = statement
  type key = timestamp
  let get_key (Statement (t, _, _, _)) = t
  let compare = Timestamp.compare
end)

type chunk = M.t

type serialized_chunk = {
  mutable loaded     : bool;
          idx        : int;
  mutable last_saved : timestamp;
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
      last_saved = Timestamp.origin ();
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
  c.loaded <- true;
  c.last_saved <- M.last_key c.data

let ensure_chunk_is_loaded c =
  if not c.loaded then load_chunk c;
  c

let rec latest statements_chunks =
  match !statements_chunks with
    | [] -> new_statements_chunks (); latest statements_chunks
    | c :: _ -> ensure_chunk_is_loaded c

let rec push s =
  try
    let chunk = latest statements_chunks in
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

type iterator =
  | End
  | Value of statement * (unit -> iterator)

let iterator_over_statements () =
  let i = M.start (latest statements_chunks).data in
  assert false

let no_more_answer = NoMoreAnswer

let single_answer x = NextAsnwers (x, fun () -> NoMoreAnswer)

let rec map_answers f = function
  | NoMoreAnswer -> NoMoreAnswer
  | Next (x, s) -> Next (f x, fun () -> map_answers f (s ()))

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

let rec answers
: type a. identifier -> interval -> a request -> statement -> a option =
fun asked_by interval r (Statement (t, who, p, x)) ->
  if not (Timestamp.mem t interval) then `Stop
  else if not (is_visible p asked_by) then `Skip
  else match r with
    | Is (p', pat) when is_visible p' asked_by ->
      begin match coerce p.ty p'.ty with
        | Eq -> match_pattern pat x
        | NEq -> `Skip
      end
    | _ ->
      `Skip

and match_pattern
: type a b. (a, b) pattern -> b -> a answers = fun p x ->
  match p, x with
    | Hole, x ->
      `Found x
    | Fst (fp, p), (fx, x) ->
      if fp = fx then
        match_pattern p x
      else
        `Skip
    | Snd (p, sp), (x, sx) ->
      if sp = sx then
        match_pattern p x
      else
        `Skip

and is_visible
: type a. a predicate -> identifier -> bool = fun p id ->
  id = daemon || filter p.visibility id

and filter
: type a. a filter -> (a -> bool) -> bool = fun f first ->
  match f with
  | True -> true
  | Satisfy r -> search ~first daemon always r

and search ?first asked_by interval r =
  let rec aux = function
    | End -> NoMoreAnswers
    | Value (x, i) ->
      match answers asked_by interval r x with
        | None -> aux (next i)
        | Some x -> match first with
            | None -> NextAnswer (x, fun () -> aux (next i))
            | Some f -> if f x then NextAnswer (x, fun () -> aux (next i))
            | _ -> aux (next i)
  in
  aux iterator_over_statements

let ask asked_by interval r =
  search asked_by interval r

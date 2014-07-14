(* -*- tuareg -*- *)

open Lwt

let ( $> ) f g () = f (); g ()

let ( !$ ) f x = f ()

let ( !* ) f x = f [x]

type 'a only = Only of 'a

let lwt_list_iteri_s f l =
  let c = ref 0 in
  Lwt_list.iter_s (fun x -> f !c x >>= fun _ -> (incr c; return ())) l

let lwt_list_join cs =
  List.fold_right (fun c xs ->
      lwt xs = xs in
      lwt x = c in return (x :: xs))
      cs
      (return [])

let lwt_list_foldmap f init xs =
  let rec aux ys accu = function
    | [] ->
      return (accu, List.rev ys)
    | x :: xs ->
      lwt (accu, y) = f accu x in
      aux (y :: ys) accu xs
  in
  aux [] init xs

let lwt_if c pt pe =
  c >>= function
    | true -> pt
    | false -> pe

let continue_while_is v ps =
  let rec aux = function
    | [] ->
      Lwt.return v
    | p :: ps ->
      lwt r = p () in
      if r = v then aux ps else Lwt.return r
  in
  aux ps

let forever what =
  let rec aux () = what aux in aux ()

let rec wait_for m p =
  lwt v = Lwt_mvar.take m in
  match p v with
    | None -> wait_for m p
    | Some x -> Lwt.return x

module type MapProduct_sig = sig
  type 'a t

  type _ prod =
    | P1 : 'a t only -> 'a only prod
    | P2 : 'a t * 'b t -> ('a * 'b) prod
    | P3 : 'a t * 'b t * 'c t -> ('a * 'b * 'c) prod
    | P4 : 'a t * 'b t * 'c t * 'd t -> ('a * 'b * 'c * 'd) prod
    | P5 : 'a t * 'b t * 'c t * 'd t * 'e t -> ('a * 'b * 'c * 'd * 'e) prod

  type tfunction = { fapply : 'a. 'a t -> 'a t }
  val map : tfunction -> 'a prod -> 'a prod

  type action = { exec : 'a. 'a t -> unit }
  val iter : action -> 'a prod -> unit

  type action2 = { exec2 : 'a. 'a t -> 'a t -> unit }
  val iter2 : action2 -> 'a prod -> 'a prod -> unit

end

module MapProduct (T : sig type 'a t end)
: MapProduct_sig with type 'a t = 'a T.t = struct
  type 'a t = 'a T.t
  type _ prod =
    | P1 : 'a t only -> 'a only prod
    | P2 : 'a t * 'b t -> ('a * 'b) prod
    | P3 : 'a t * 'b t * 'c t -> ('a * 'b * 'c) prod
    | P4 : 'a t * 'b t * 'c t * 'd t -> ('a * 'b * 'c * 'd) prod
    | P5 : 'a t * 'b t * 'c t * 'd t * 'e t -> ('a * 'b * 'c * 'd * 'e) prod

  type tfunction = { fapply : 'a. 'a t -> 'a t }

  let map (type a) (f : tfunction) : a prod -> a prod = function
    | P1 (Only x) ->
      P1 (Only (f.fapply x))
    | P2 (x, y) ->
      P2 (f.fapply x, f.fapply y)
    | P3 (x, y, z) ->
      P3 (f.fapply x, f.fapply y, f.fapply z)
    | P4 (x, y, z, t) ->
      P4 (f.fapply x, f.fapply y, f.fapply z, f.fapply t)
    | P5 (x, y, z, t, u) ->
      P5 (f.fapply x, f.fapply y, f.fapply z, f.fapply t, f.fapply u)

  type action = { exec : 'a. 'a t -> unit }

  let iter (type a) (f : action) (p : a prod) : unit =
    match p with
      | P1 (Only x) ->
        f.exec x
      | P2 (x, y) ->
        f.exec x; f.exec y
      | P3 (x, y, z) ->
        f.exec x; f.exec y; f.exec z
      | P4 (x, y, z, t) ->
        f.exec x; f.exec y; f.exec z; f.exec t
      | P5 (x, y, z, t, u) ->
        f.exec x; f.exec y; f.exec z; f.exec t; f.exec u

  type action2 = { exec2 : 'a. 'a t -> 'a t -> unit }

  let iter2 (type a) (f : action2) (p : a prod) (q : a prod) =
    match (p, q) with
      | P1 (Only x), P1 (Only x') ->
        f.exec2 x x'
      | P2 (x, y), P2 (x', y') ->
        f.exec2 x x'; f.exec2 y y'
      | P3 (x, y, z), P3 (x', y', z') ->
        f.exec2 x x'; f.exec2 y y'; f.exec2 z z'
      | P4 (x, y, z, t), P4 (x', y', z', t') ->
        f.exec2 x x'; f.exec2 y y'; f.exec2 z z'; f.exec2 t t'
      | P5 (x, y, z, t, u), P5 (x', y', z', t', u') ->
        f.exec2 x x'; f.exec2 y y'; f.exec2 z z'; f.exec2 t t'; f.exec2 u u'
      | _, _ -> assert false

end

let proj_1_3 (x, _, _) = x
let proj_2_3 (_, x, _) = x
let proj_3_3 (_, _, x) = x

let update_assoc k v l =
  let l = List.remove_assoc k l in
  (k, v) :: l

let opt_assoc k l =
  try Some (List.assoc k l) with Not_found -> None

let map_assoc k f l =
  match opt_assoc k l with
    | Some x -> update_assoc k (f x) l
    | None -> l

let cons_if cond x xs =
  if cond then x :: xs else xs

let lwt_repeat k f =
  let rec aux i = if i = 0 then return () else f (k - i) >>= fun _ -> aux (pred i) in
  aux k

let lwt_nat_stream () =
  let state = ref 0 in
  Lwt_stream.from (fun () -> incr state; return (Some !state))

let range start stop =
  let rec aux k =
    if k >= stop then [] else k :: aux (k + 1)
  in
  aux start

let list_foldmap f init xs =
  let rec aux ys accu = function
    | [] ->
      (accu, List.rev ys)
    | x :: xs ->
      let (accu, y) = f accu x in
      aux (y :: ys) accu xs
  in
  aux [] init xs

let list_take =
  let rec aux k l =
    if k = 0 then l
    else match l with
    | [] -> []
    | x :: xs -> x :: aux (pred k) xs
  in
  aux

let list_index_of k =
  let rec find i = function
    | [] -> raise Not_found
    | x :: xs when x = k -> i
    | _ :: xs -> find (succ i) xs
  in
  find 0

let list_remove i l =
  let rec aux k = function
    | [] -> []
    | x :: xs -> if k = i then xs else x :: aux (k + 1) xs
  in
  aux 0 l

let list_replace i v l =
  let rec aux k = function
    | [] -> [v]
    | x :: xs -> if k = i then v :: xs else x :: aux (k + 1) xs
  in
  aux 0 l

let list_cut n l =
  let rec aux k = function
    | [] -> []
    | x :: xs when k > 0 -> aux (pred k) xs
    | xs -> xs
  in
  aux n l

let list_tl_cut n l =
  List.rev (list_cut n (List.rev l))

let lwt_condition_wait_timeout timeout c =
  let timeout_waiter, timeout_wakener = task () in
  let t = Lwt_timeout.create timeout (fun () -> wakeup timeout_wakener None) in
  Lwt_timeout.start t;
  lwt result =
    pick [ (try_lwt timeout_waiter with _ -> return None) ;
           (Lwt_condition.wait c >>= (fun x -> return (Some x)))
         ]
  in
  Lwt_timeout.stop t;
  return result

let natural_indices () =
  let indices = Hashtbl.create 13 in
  let invariant () =
    let max = Hashtbl.fold (fun _ v v' -> max v v') indices (-1) in
    let a = Array.create (max + 1) 0 in
    let marked_exactly_once = ref true and marks = ref (-1) in
    let mark_exactly_once i =
      if a.(i) <> 0 then
        marked_exactly_once := false
      else
        incr marks
      ;
      a.(i) <- a.(i) + 1
    in
    Hashtbl.iter (fun _ i -> mark_exactly_once i) indices;
    if not (!marked_exactly_once && !marks = max) then (
      let string_of_marks a =
        let s = ref "" in
        Array.iteri (fun i c ->
          s := !s ^ " " ^ string_of_int i ^ " " ^ string_of_int c)
          a;
        !s
      in
      Printf.eprintf "Broken invariant: %d <> %d or [%s] is wrong (%B)."
        !marks max (string_of_marks a) !marked_exactly_once;
      assert false
    )
  in
  let on_indices f x = invariant (); let y = f x in invariant (); y in
  let indices_remove = on_indices (fun elt ->
    try
      let idx = Hashtbl.find indices elt in
      Hashtbl.remove indices elt;
      Hashtbl.iter (fun elt idx' ->
        if idx' > idx then Hashtbl.replace indices elt (idx' - 1)
      ) indices
    with Not_found -> (* Should never happen. *) assert false
  ) in
  let indices_insert elt = on_indices (fun idx ->
    Hashtbl.iter (fun elt idx' ->
      if idx' >= idx then Hashtbl.replace indices elt (idx' + 1)
    ) indices;
    Hashtbl.add indices elt idx
  ) in
  let indices_fresh_for id = on_indices (fun idx ->
    Hashtbl.add indices id idx;
    id
  ) in
  (indices_remove, indices_insert, Hashtbl.find indices, indices_fresh_for)

module ExtFilename = struct

  let temp_filename ?(temp_dir = Filename.get_temp_dir_name ()) prefix suffix =
    let fname = Filename.temp_file ~temp_dir prefix suffix in
    Lwt_unix.unlink fname
    >>= fun _ -> return fname

end

type ('a, 'e) exn_free =
  [`OK of 'a | `KO of 'e] Lwt.t

type ('a, 'b, 'e) exn_abs =
    (('e -> 'b Lwt.t) -> 'a Lwt.t)

exception LocalError

let ltry what =
  let error = ref None in
  let lraise e =
    error := Some e;
    raise_lwt LocalError
  in
  try_lwt
    lwt r = what lraise in
    return (`OK r)
  with LocalError ->
    match !error with
      | None -> assert false
      | Some e ->
        return (`KO e)

let lreturn x = fun _ -> return x

let do_not_fail f =
  try
    f (); return (`OK ())
  with (Assert_failure _) as e ->
    return (`KO (`AssertFailure (Printexc.to_string e)))

let ( !!> ) (f : unit -> [`OK of 'a | `KO of 'e] Lwt.t) lraise =
  f () >>= function
    | `OK x -> return x
    | `KO e -> lwt _ = lraise e in assert false

exception SmallJump

let small_jump _ =
  raise_lwt SmallJump

let warn_only w =
  ignore (Log.log w);
  small_jump

let ( @* ) f x = fun () -> f x

let ( >>> ) e f =
  fun l -> e l >>= fun _ -> f l

let ( >-> ) e f =
  fun l -> e l >>= (fun x -> f x l)

let ( !>> ) e =
  e

let ( !>>> ) e =
  e

let ( !>>= ) e =
  e ()

let ( >>>= ) p1 p2 =
  p1 >>= function
    | `OK x -> p2 x
    | `KO e -> return (`KO e)

let rec list_map_s f = function
  | [] ->
    return (`OK [])
  | x :: xs ->
    f x >>>= (fun fx ->
      list_map_s f xs >>>= (fun fxs ->
        return (`OK (fx :: fxs))
      )
    )

let ( @| ) e p =
  try_lwt
    lwt _ = e () in
    p ()
  with SmallJump ->
    p ()

let first_success f error xs =
  let rec aux es = function
    | [] -> return (`KO (error es))
    | x :: xs ->
      f x >>= function
        | `OK x -> return (`OK x)
        | `KO e -> aux (e :: es) xs
  in
  aux [] xs

module MRef = struct
  open Lwt_mutex
  type 'a t = { mutable content : 'a; mutex : Lwt_mutex.t }
  let create x = { content = x; mutex = Lwt_mutex.create () }
  let read x f = with_lock x.mutex (fun () -> f x.content)
  let write x v = with_lock x.mutex (fun () -> return (x.content <- v))
end

let string_of_date d = Unix.(
  let d = localtime d in
  Printf.sprintf "%04d/%02d/%02d %02d:%02d:%02d"
    (1900 + d.tm_year) (1 + d.tm_mon) d.tm_mday
    d.tm_hour d.tm_min d.tm_sec
)

type 'a oref = 'a option ref
let oref (type a) failure : (a -> unit) * (unit -> a) =
  let r = ref None in
  let set x = r := Some x in
  let get () = match !r with
    | None -> failure ()
    | Some x -> x
  in
  set, get

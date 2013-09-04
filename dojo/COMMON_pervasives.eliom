(* -*- tuareg -*- *)

{shared{

let ( $> ) f g () = f (); g ()

let ( !$ ) f x = f ()

let ( !* ) f x = f [x]

type 'a only = Only of 'a

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

}}

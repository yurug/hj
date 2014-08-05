type ('s, 'a) response =
| Done
| Yield of 'a * 's
| Skip of 's

type ('s, 'a) internal_stream =
  's * ('s -> ('s, 'a) response)

type 'a stream =
  Stream : ('s, 'a) internal_stream -> 'a stream

exception ExhaustedStream

let foldn n f init s =
  let rec aux accu n (Stream (state, next)) =
    if n = 0 then accu else
      match next state with
        | Done -> raise ExhaustedStream
        | Skip state -> aux accu n (Stream (state, next))
        | Yield (x, state) -> aux (f x accu) (pred n) (Stream (state, next))
  in
  aux init n s

let take n s = List.rev (foldn n (fun x l -> x :: l) [] s)

let from_fun init f = Stream (init, fun accu -> f accu)

let map f (Stream (state, next)) =
  let mnext state =
    match next state with
      | Skip state -> Skip state
      | Done -> Done
      | Yield (x, state) -> Yield (f x, state)
  in
  Stream (state, mnext)

type ('a, 'b) either = Left of 'a | Right of 'b

let concat (Stream (state1, next1)) (Stream (state2, next2)) =
  let next = function
    | Left state1 ->
      begin match next1 state1 with
        | Done -> Skip (Right state2)
        | Yield (x, state1) -> Yield (x, Left state1)
        | Skip state1 -> Skip (Left state1)
      end
    | Right state2 ->
      begin match next2 state2 with
        | Done -> Done
        | Yield (x, state2) -> Yield (x, Right state2)
        | Skip state2 -> Skip (Right state2)
      end
  in
  Stream (Left state1, next)

let single x = Stream (false, function true -> Done | false -> Yield (x, true))

let from_list l =
  let next = function
    | [] -> Done
    | x :: xs -> Yield (x, xs)
  in
  Stream (l, next)

let is_empty s = try ignore (take 1 s); true with ExhaustedStream -> true

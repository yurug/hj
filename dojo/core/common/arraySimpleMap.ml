module type OrderedByKeyData = sig
  type key
  val compare : key -> key -> int
  type data
  val get_key : data -> key
end

module Make (S : OrderedByKeyData) = struct

  open S

  type t = {
    data             : data option array;
    mutable last_idx : int;
  }

  let make size =
    let size = max 0 size in
    {
      data = Array.make size None;
      last_idx = -1;
    }

  exception Full
  exception MisOrdered of data * data
  exception InvalidIndex of int

  let get m idx =
    match m.data.(idx) with
      | None -> raise (InvalidIndex idx)
      | Some x -> x

  let last m = get m (m.last_idx)

  let after_last_key m k =
    m.last_idx = -1 || compare (get_key (last m)) k <> 1

  let insert m d =
    let k = get_key d in
    if not (after_last_key m k) then raise (MisOrdered (last m, d))
    else if Array.length m.data <= m.last_idx + 1 then raise Full
    else (
      m.last_idx <- m.last_idx + 1;
      m.data.(m.last_idx) <- Some d
    );
    m.last_idx

  let length m = m.last_idx + 1

  exception Not_found

  let find m k =
    let mid m n = (m + n) / 2 in
    let rec aux i start stop =
      if stop < start then raise Not_found else
        let d = get m i in
        let c = compare (get_key d) k in
        if c = 0 then (i, d)
        else if c > 0 then continue start (i - 1)
        else continue (i + 1) stop
    and continue start stop =
      aux (mid start stop) start stop
    in
    aux (mid 0 m.last_idx) 0 m.last_idx

end

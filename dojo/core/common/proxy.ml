(* FIXME: We may want an expiration date on keys. *)

module Make (H : Hashtbl.HashedType) = struct
  type key = int * float
  deriving (Json)

  module T = Hashtbl.Make (H)
  module K = Hashtbl.Make (struct
    type t = key
    let equal x y = (compare x y = 0)
    let hash (k, _) = k
  end)

  type table = {
    htable : key T.t;
    ktable : H.t K.t;
  }

  let make () = {
    htable = T.create 13;
    ktable = K.create 13;
  }

  let fresh_key =
    let c = ref 0 in
    fun () ->
      incr c;
      (!c, Unix.gettimeofday ())

  let cache t x =
    try
      T.find t.htable x
    with Not_found ->
      let k = fresh_key () in
      K.add t.ktable k x;
      T.add t.htable x k;
      k

  let deref t k =
    K.find t.ktable k

end

(* FIXME: We may want an expiration date on keys. *)

module Make (H : sig
  include Hashtbl.HashedType
  val size : t -> int
end) = struct
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

  let cache_size t =
    let s = ref 0 in
    K.iter (fun k v -> s := !s + H.size v) t.ktable;
    T.iter (fun k v -> s := !s + H.size k) t.htable;
    !s

  let show_cache_size_freq = 100
  let debug_show_cache_size =
    let c = ref 0 in
    fun t ->
      try
	incr c;
	if !c mod show_cache_size_freq = 0 then
	  Printf.eprintf "CACHE SIZE: %d\n%!" (cache_size t)
      with _ -> ()

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

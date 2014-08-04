open ArraySimpleMap
open Testsuite
open Lwt

module O = struct
  type key = int
  type data = int * string
  let get_key = fst
  let compare = compare
end

module A = Make (O)

exception Unexpected of string * string

let expected e v =
  if e <> v then raise (Unexpected (e, v))

let basic_usage () =
  test "Manual insertions and lookups" A.(fun () ->
    let a = make 5 in
    let _idx1 = insert a (42, "v1") in
    let _idx2 = insert a (43, "v2") in
    expected (snd (snd (find a 42))) "v1";
    expected (snd (snd (find a 43))) "v2";
    return ()
  )

let composite_usage () =
  test "Manual concatenation" A.(fun () ->
    let a = make 5 and b = make 5 in
    let _idx1 = insert a (42, "v1") in
    let _idx2 = insert b (43, "v2") in
    let _idx3 = insert_map a b in
    expected (snd (snd (find a 42))) "v1";
    expected (snd (snd (find a 43))) "v2";
    return ()
  ) >=> fun () -> test "Manual extraction" A.(fun () ->
    let a = make 5 in
    let _idx1 = insert a (42, "v1") in
    let _idx2 = insert a (43, "v2") in
    let b = sub a 42 42 in
    expected (snd (snd (find b 42))) "v1";
    return ()
  )

let random_usage () =
  test "Random monotonic insertions and lookups" A.(fun () ->
    let a = make (1024 + Random.int 128) in
    let start = ref 0 in
    let keys = ref [] in
    for i = 0 to 1023 do
      start := !start + Random.int 500;
      keys := !start :: !keys;
      ignore (insert a (!start, string_of_int !start))
    done;
    List.iter (fun k ->
      expected (snd (snd (find a k))) (string_of_int k)
    ) !keys;
    return ()
  )

let random_composite_usage () =
  test "Concatenation of two random maps" A.(fun () ->
    let a = make (1024 + Random.int 128) in
    let b = make (1024 + Random.int 128) in
    let start = ref 0 in
    let keys = ref [] in
    let insert_keys c =
      for i = 0 to 512 do
        start := !start + Random.int 500;
        keys := !start :: !keys;
        ignore (insert c (!start, string_of_int !start))
      done
    in
    insert_keys a;
    insert_keys b;
    ignore (insert_map a b);
    List.iter (fun k ->
      expected (snd (snd (find a k))) (string_of_int k)
    ) !keys;
    return ()
  )

let limit_composite_usage () =
  test "Concatenation of an empty map" A.(fun () ->
    let a = make 5 and b = make 5 in
    let _idx1 = insert a (42, "v1") in
    let _idx3 = insert_map a b in
    expected (snd (snd (find a 42))) "v1";
    return ()
  ) >=> fun () -> test "Concatenation to an empty map" A.(fun () ->
    let a = make 5 and b = make 5 in
    let _idx1 = insert b (42, "v1") in
    let _idx3 = insert_map a b in
    expected (snd (snd (find a 42))) "v1";
    return ()
  )

let run () =
  scenario "Array Simple Map" (fun () ->
    !=> basic_usage
    >=> composite_usage
    >=> random_usage
    >=> random_composite_usage
    >=> limit_composite_usage
  )

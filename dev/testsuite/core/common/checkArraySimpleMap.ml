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

let run () =
  scenario "Array Simple Map" (fun () ->
    !=> basic_usage
    >=> random_usage
  )


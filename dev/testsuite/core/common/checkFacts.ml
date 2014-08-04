open Testsuite
open Lwt
open Facts

let basic_usage () =
  test "Manual insertions of facts and requests about them." (fun () ->
    let p1 = make_predicate "P" True True TInt in
    let me = Identifier.identifier_of_string_list ["me"] in
    let _ = state me p1 42 in
    let _ = state me p1 0 in
    return ()
  )

let run () =
  scenario "Facts" (fun () ->
    !=> basic_usage
  )

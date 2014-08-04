open Facts
open Testsuite
open Lwt

let basic_usage () =
  test "Manual insertions of facts and requests about them." (fun () ->
    return ()
  )

let run () =
  scenario "Facts" (fun () ->
    !=> basic_usage
  )

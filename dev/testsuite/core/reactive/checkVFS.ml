open Testsuite
open VFS
open Lwt

let run () =
  scenario "VFS basic usage" (fun () ->

    let tmp = Filename.temp_file "hj" "testdir" in

    test "Initialization of VFS" (fun () ->
      init_root tmp
    )
  )

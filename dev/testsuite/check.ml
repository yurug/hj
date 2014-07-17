let run () =
  CheckCore.run ()

let main =
  run ();
  Testsuite.final_score ()

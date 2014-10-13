(* -*- tuareg -*- *)

let server_function' json f =
  server_function Json.t<string> (fun str ->
    f (Deriving_Json.from_string json str)
  )

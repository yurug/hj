(* -*- tuareg -*- *)
{shared{
open Lwt
}}
open ExtPervasives
open WidgetHTML

let export_as_pdf =
  ExtEliom.server_function' Json.t<Questions.t> (fun questions ->
    let tex = QuestionsLaTeX.make questions in
    let tmp = Filename.temp_file "hj" ".pdf" in
    (ltry (ExtUnix.pdflatex tex tmp)) >>= function
      | `OK _ -> FileHTTP.send tmp >>= fun url -> return (Some url)
      | `KO _ -> return None
  )

let download_as_pdf questions =
  let questions = Deriving_Json.to_string Json.t<Questions.t> questions in
  small_button [I18N.(String.(cap download_pdf))] {unit -> unit{
    fun _ -> Lwt.async (fun () ->
      %export_as_pdf %questions >>= function
        | None ->
          return ()
        | Some url ->
          return (Dom_html.window##location##assign (Js.string url))
    )
  }}

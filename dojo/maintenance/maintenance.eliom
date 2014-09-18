{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
}}

module Maintenance_app =
  Eliom_registration.App (
    struct
      let application_name = "maintenance"
    end)

let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let () =
  Maintenance_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"maintenance"
           ~css:[["css";"maintenance.css"]]
           Html5.F.(body [
             h2 [pcdata "Le Hackojo est actuellement en maintenance..."];
             h2 [pcdata "The Hackojo is currently being fixed..."];
           ])))

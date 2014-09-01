(* -*- tuareg -*- *)

{client{

  let get_element_by_id id =
    Js.coerce_opt (
      Dom_html.document##getElementById (id)
    ) (fun x -> Js.some x) (fun _ -> assert false)

  let get_input_by_id id =
    Js.coerce_opt (
      Dom_html.document##getElementById (id)
    ) Dom_html.CoerceTo.input (fun _ -> assert false)

}}

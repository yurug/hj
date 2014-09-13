(* -*- tuareg -*- *)

{client{

  open Dom_html

  let get_node_by_id f id =
    Js.coerce_opt
      (document##getElementById (Js.string id))
      f
      (fun _ -> assert false)

  let get_element_by_id = get_node_by_id (fun x -> Js.some x)
  let get_div_by_id     = get_node_by_id CoerceTo.div
  let get_input_by_id   = get_node_by_id CoerceTo.input

}}

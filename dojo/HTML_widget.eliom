(* -*- tuareg -*- *)

{shared{

  open Lwt

  open Eliom_content
  open Html5
  open Html5.D
  open Html5_types

  open COMMON_pervasives

  type onclick_cb = (Dom_html.mouseEvent Js.t -> unit) client_value

  let generic_button classes labels onclick =
    let id = Id.new_elt_id ~global:true () in
    let onclick = {{
      let state = ref 0 in
      let next () =
        if !state = List.length %labels - 1 then state := 0 else incr state
      in
      let label () = List.nth %labels !state in
      fun e ->
        let open Eliom_content.Html5 in
        next ();
        Manip.Named.replaceAllChild %id [label ()];
        %onclick e
    }}
    in
    let label = List.nth labels 0 in
    Id.create_named_elt ~id (
      span ~a:[a_onclick onclick; a_class classes] [label]
    )

  let button ls = generic_button ["button"] (List.map (fun l -> pcdata l) ls)
  let icon = generic_button ["icon"]

  type show_state =
    | Hidden of string
    | Shown

}}

(* FIXME: Maybe we should define an abstraction for editable
   objects. *)

type editable_list = {
  fields    : string list;
  index_end : unit -> int Lwt.t;
  display   : int -> string list Lwt.t;
  remove    : (int -> string list -> unit Lwt.t) option;
  replace   : (int -> string list -> unit Lwt.t) option;
}

let list_editor label list =

  (** A table to convert HTML5 identifier into list indices. *)

  let indices_remove, indices_insert, indices_get, indices_fresh_for =
    natural_indices ()
  in

  (* FIXME: I would have liked to generalize the following function wrt
     the type of the server-side action but I failed to have the
     polymorphic version accepted by the type checker because of some
     type variable escaping that is hard to understand because it is
     related to the way eliom splits code into server-side and
     client-side code fragments. *)

  (** [list_action f e pre post] returns the client callback corresponding
      to the following computation:
      [
        lwt xc = pre () in            (** On the client side. *)
        lwt last_modified = f xc e in (** On the server side. *)
        post last_modified            (** On the client side. *)
      ]

      [last_modified] indicates if [e] is the last element of
      the list, i.e. the slot of the editor used to define new elements.
  *)
  let list_action
      (f : (int -> string list -> unit Lwt.t) option)
      elt
      (pre_action  : (unit -> (string list) Lwt.t) client_value)
      (post_action : (bool -> unit Lwt.t) client_value) =
    match f with
      | None ->
        {{ fun _ -> () }}

      | Some f ->
        let code = server_function Json.t<string list> (fun xc ->
          try
            lwt e = list.index_end () in
            let idx = indices_get elt in
            f idx xc >> return (idx = e)
          with Not_found -> assert false
        )
        in
        {{ fun _ -> Lwt.async (fun () ->
          %pre_action () >>= fun xc -> %code xc >>= %post_action
        )}}
  in

  (** Actions. *)

  let rec remove_item table elt =
    let remove_elt = server_function Json.t<unit> (fun () ->
      return (indices_remove elt)
    ) in
    list_action list.remove elt {{ fun () -> return [] }} {{ fun on_last_idx ->
      if not on_last_idx then (
        let row = Id.get_element %elt in
        Manip.Named.removeChild %table row;
        %remove_elt ()
      ) else return ()
    }}

  and replace_item table elt =
    let get_rows = {{ fun () ->
      let text_of_td ts (e : Dom.node Js.t) =
        let unpack e f =
          let c = e##firstChild in
          Js.Opt.case c (fun () -> ts) f
        in
        unpack e (fun e -> unpack e (fun e ->
          Js.Opt.case (Dom.CoerceTo.text e) (fun () -> ts) (fun v ->
            let s = v##data in
            Js.to_string s :: ts
          )))
      in
      let row = Id.get_element %elt in
      let tds = (To_dom.of_tr row)##childNodes in
      return (List.(rev (list_cut 1 (
        fold_left text_of_td [] (Dom.list_of_nodeList tds)
      ))))
    }} in
    let fresh_row_if_needed = fresh_row_if_needed table elt in
    let after_replace = {bool -> unit Lwt.t{ fun on_last_idx ->
      %fresh_row_if_needed on_last_idx >>= function
        | None -> return ()
        | Some tr ->
          let tb = Id.get_element %table in
          let tb = To_dom.of_tbody tb in
          ignore (tb##appendChild (((To_dom.of_tr tr) :> Dom.node Js.t)));
          return ()
    }}
    in
    list_action list.replace elt get_rows after_replace

  and fresh_row_if_needed table elt =
    server_function Json.t<bool> (fun on_last_idx ->
      if on_last_idx then
        lwt e = list.index_end () in
        lwt tr = row_of_idx table e in
        return (Some tr)
      else
        return None
    )

  and tools_of table elt =
    let remove_action  = icon [pcdata "x"] (remove_item table elt) in
    let replace_action = icon [pcdata "✓"] (replace_item table elt) in
    td [ remove_action; replace_action ]

  and cells_of_tr fields table id =
    List.map field fields @ [tools_of table id]

  and field f =
    (* FIXME: A workaround to the following bug of Ocsigen:
       https://github.com/ocsigen/tyxml/commit
       /1a05bd9e7f96720b3a57289054ab9c4a5fd9926a#commitcomment-4425462 *)
    let elt = span [pcdata f] in
    ignore {unit{
      let e = To_dom.of_span %elt in
      e##setAttribute (Js.string "contenteditable", Js.string "true")
    }};
    td [elt]

  and row_of_idx table i =
    lwt fields = list.display i in
    let id = indices_fresh_for (Id.new_elt_id ()) i in
    let cells = cells_of_tr fields table id in
    return (Id.create_named_elt ~id (tr cells))
  in
  let tableid = Id.new_elt_id () in
  lwt e = list.index_end () in
  lwt rows = Lwt_list.map_s (row_of_idx tableid) (range 0 (e + 1)) in
  let thead = thead [ tr (
    List.map (fun f -> th [pcdata f]) list.fields
    @ [th [pcdata ""]]
  ) ]
  in
  let table = (tablex ~thead [Id.create_named_elt ~id:tableid (tbody rows)]) in
  return (div [table])


{client{
  let toggle s e =
    match !s with
      | Shown ->
        s := Hidden (Html5.Manip.Css.display e);
        Manip.SetCss.display e "none"
      | Hidden d ->
        Manip.SetCss.display e d;
        s := Shown
}}

{shared{

  let show_or_hide ?(start_shown=true) (e : [ body_content_fun ] elt) =
    let e = Id.create_global_elt e in
    let see : [ body_content_fun ] elt =
      let labels = [I18N.cap I18N.String.hide; I18N.cap I18N.String.see] in
      let labels = if start_shown then labels else List.rev labels in
      button labels {Dom_html.mouseEvent Js.t -> unit{
        let s = ref Shown in
        if not %start_shown then toggle s %e;
        fun (_ : Dom_html.mouseEvent Js.t) -> toggle s %e
      }}
    in
    (see, e)

}}

let always_valid : (string -> string option) client_value =
  {{ fun (_ : string) -> (None : string option) }}

let nonempty_field : (string -> string option) client_value =
  {{ fun (s : string) ->
    if String.length s = 0 then
      Some I18N.String.this_field_must_not_be_empty
    else
      None
   }}

let validate_input validator id =
  match validator with
    | None ->
      ([], [])
    | Some validator ->
      let message = span [] in
      let validator =
        {{
          let open Eliom_content.Html5 in
              let nb = ref 0 in
              fun _ ->
                incr nb;
                let nb_now = !nb in
                let input_elt = Id.get_element %id in
                let input_value = (To_dom.of_input input_elt)##value in
                Lwt.async (fun () -> Lwt_js.sleep 0.5 >>
                  if !nb = nb_now then
                    let v =
                      match %validator (Js.to_string input_value) with
                        | None -> "✓ OK"
                        | Some reason -> "❌ " ^ reason
                    in
                    Lwt.return (
                      Manip.replaceAllChild %message [pcdata v]
                    ) else Lwt.return ()
                )
        }}
      in
      ([ a_oninput validator; a_onchange validator ], [message])

let field
    id name
    ?validator
    ?(fieldname : [ `Input ] Id.id option) input_type text =
  let input_id : [> `Input ] Id.id =
    match fieldname with
      | None -> Id.new_elt_id ~global:true ()
      | Some id -> (id :> [ `Input ] Id.id)
  in
  let input_validator, message =
    validate_input validator input_id
  in
  let input =
    Id.create_named_elt input_id  (
      string_input ~a:input_validator ~input_type ~name ()
    )
  in
  div ~a:[a_id id] ([
    label ~a:[a_for name] [ pcdata text ];
    (input :> [ div_content ] elt)
  ] @ message)

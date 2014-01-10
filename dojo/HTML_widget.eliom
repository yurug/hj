(* -*- tuareg -*- *)

{shared{

  open Lwt

  open Eliom_content
  open Html5
  open Html5.D
  open Html5_types

  open COMMON_pervasives

  type onclick_cb = (unit -> unit) client_value

  let generic_button classes labels onclick =
    let id = Id.new_elt_id ~global:false () in
    let onclick = {onclick_cb{
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
      div ~a:[a_onclick {{ fun _ -> %onclick () }}; a_class ("inlined" :: classes) ] [label]
    )

  let small_button ls = generic_button ["menu_button"] (List.map (fun l -> pcdata l) ls)
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

let list_editor
    ?(no_header    = false)
    ?(no_action    = false)
    ?(no_insertion = false)
    list
    (extra_actions : int -> _)
    (cell_status : int -> int -> [`RO | `RW])
=

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
      (post_action : (bool -> unit Lwt.t) client_value) : onclick_cb =
    match f with
      | None ->
        {{ fun () -> () }}

      | Some f ->
        let code = server_function Json.t<string list> (fun xc ->
          try
            lwt e = list.index_end () in
            let idx = indices_get elt in
            f idx xc >>= fun _ -> return (idx = e)
          with Not_found -> assert false
        )
        in
        {{ fun () -> Lwt.async (fun () ->
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

  and replace_item no_action table elt =
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
      let select =
        if %no_action then
          fun x -> List.rev x
        else
          fun l -> List.(rev (list_cut 1 l))
      in
      return (select (List.fold_left text_of_td [] (Dom.list_of_nodeList tds)))
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

  and tools_of table id idx =
    let remove_action  = icon [pcdata "x"] (remove_item table id) in
    let replace_action = icon [pcdata "✓"] (replace_item no_action table id) in
    td ([ remove_action; replace_action ] @ extra_actions idx)

  and cells_of_tr a fields table id idx =
    List.mapi (field a idx) fields
    @ (if no_action then [] else [tools_of table id idx])

  and field a row col f =
    (* FIXME: A workaround to the following bug of Ocsigen:
       https://github.com/ocsigen/tyxml/commit
       /1a05bd9e7f96720b3a57289054ab9c4a5fd9926a#commitcomment-4425462 *)
    let editable = list.replace <> None && cell_status row col = `RW in
    let a = if editable then a_class ["editable_cell"] :: a else a in
    let elt = span ~a [pcdata f] in
    if editable then
      ignore {unit{
        let e = To_dom.of_span %elt in
        e##setAttribute (Js.string "contenteditable", Js.string "true")
      }};
    td [elt]

  and row_of_idx table i =
    lwt fields = list.display i in
    let id = indices_fresh_for (Id.new_elt_id ~global:false ()) i in
    (** When no user action is enabled, editing means replacing. *)
    let a =
      if no_action then
        let change = replace_item no_action table id in
        [a_oninput {{ fun _ -> %change () }} ]
      else
        []
    in
    let cells = cells_of_tr a fields table id i in
    return (Id.create_named_elt ~id (tr cells))
  in
  let tableid = Id.new_elt_id ~global:false () in
  lwt e = list.index_end () in
  let r = range 0 (if no_insertion then e else e + 1) in
  lwt rows = Lwt_list.map_s (row_of_idx tableid) r in
  let table =
    if no_header then
      tablex [Id.create_named_elt ~id:tableid (tbody rows)]
    else
      let thead = thead [ tr (
        List.map (fun f -> th [pcdata f]) list.fields
        @ [th [pcdata "Action"]] ) ]
      in
      tablex ~thead [Id.create_named_elt ~id:tableid (tbody rows)]
  in
  return (div [table])

let server_get_list_editor
    ?(no_header    = false)
    ?(no_action    = false)
    ?(no_insertion = false)
    fields get set extra_actions cell_status =
  let rd f = lwt l = get () in f l in
  let empty = List.map (fun _ -> "") fields in
  let remove, replace =
    match set with
      | Some set ->
        let wr f = rd (fun l -> set (f l)) in
        let remove = Some (fun i _ -> wr (list_remove i)) in
        let replace = Some (fun i vs -> wr (fun l -> list_replace i vs l)) in
        (remove, replace)
      | None ->
        (None, None)
  in
  let desc = List.({
      fields;
      index_end = (fun () -> rd (fun l -> return (length l)));
      display   = (fun k -> rd (fun l -> return (try nth l k with _ -> empty)));
      remove;
      replace;
    })
  in
  list_editor
    ~no_header ~no_action ~no_insertion
    desc extra_actions cell_status

let get_list_editor
    ?(no_header    = false)
    ?(no_action    = false)
    ?(no_insertion = false)
    fields get set extra_actions cell_status =
  server_function Json.t<unit> (fun () ->
    server_get_list_editor ~no_header ~no_action ~no_insertion
      fields get set extra_actions cell_status
  )

let get_choices_editor choices add del =
  let choice_item idx c =
    let idx = succ idx in
    let change_choice =
      let choice = ref false in
      fun _ ->
        if !choice then del idx else add idx;
        choice := not !choice
    in
    let change_choice_cb = server_function Json.t<unit> (fun () ->
      Lwt.return (change_choice ())
    )
    in
    p [input
          ~a:[a_onclick {{ fun _ -> Lwt.async (fun () ->
              %change_choice_cb ())
             }}]
          ~input_type:`Checkbox();
       pcdata c]
  in
  server_function Json.t<unit> (fun () ->
    return (div (List.mapi choice_item choices))
  )

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
      button labels {unit -> unit{
        let s = ref Shown in
        if not %start_shown then toggle s %e;
        fun () -> toggle s %e
      }}
    in
    (see, e)

}}

let always_valid : (string -> string option) client_value =
  {{ fun (_ : string) -> (None : string option) }}

let nonempty_field:  (string -> string option) client_value =
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
                Lwt.async (fun () -> Lwt_js.sleep 0.5 >>= fun _ ->
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
      | None -> Id.new_elt_id ~global:false ()
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

open Ocsigen_extensions

(* FIXME: Implement this using the HTML5 file API.
   For the moment, js_of_ocaml seems not to provide
   enough on that aspect...
*)

let file_upload_service import =
  Eliom_registration.Action.register_post_coservice'
    ~post_params:(Eliom_parameter.(file "file"))
    (fun () file ->
      Ocsigen_messages.errlog
        (Printf.sprintf "Received a file (%Ld):\n %s\n%s\n%s"
           file.Ocsigen_extensions.filesize
           file.Ocsigen_extensions.tmp_filename
           file.Ocsigen_extensions.raw_original_filename
           file.Ocsigen_extensions.original_basename);
        (* FIXME: Handle error. *)
      lwt dest, commit = import file.original_basename in
      ltry COMMON_unix.(cp file.tmp_filename dest)
      >>= fun _ -> commit ()
    )

type ('a, 'b, 'c) c =
    ?a:'a Eliom_content.Html5.D.attrib list ->
    'b Eliom_content.Html5.D.elt list -> 'c Eliom_content.Html5.D.elt

let fileuploader_wrapper width height import (constructor : (_, _, _) c) body =
  let form_id = Id.new_elt_id ~global:false () in
  let onchange = {{ fun _ ->
    let f = Id.get_element %form_id in
    let f = To_dom.of_form f in
    f##submit ()
  }}
  in
  let style =
    Printf.sprintf
      "width:%fem; height:%fem; position:absolute; text-align:center;"
      width height
  in
  Id.create_named_elt form_id (
    post_form ~a:[a_class ["inlined"]]
      ~xhr:true
      ~service:(file_upload_service import) (fun f -> [
        constructor ~a:[a_class ["fileContainer"; "inlined"]; a_style style] [
          file_input
            ~a:[a_onchange onchange; a_class ["inlined"]; a_style style]
            ~name:f ();
          body
        ];
      ]) ()
  )

let fileuploader width height import =
  fileuploader_wrapper width height import
    (label : _ :> (_, _, _) c)
    (pcdata "↑")

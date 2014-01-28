(* -*- tuareg -*- *)

(** The Hackojo web application. *)

open Eliom_content.Html5.D
open Html5_types

(** This  module register  the Web application  in Eliom  and provides
    common definitions  shared between all  the HTML pages of  the web
    interface. *)

(** The Web application. *)
module Hackojo_app : Eliom_registration.ELIOM_APPL

(** [hackojo_page elts] produces an  HTML page containing  [elts] and
    everything  that  is   common  the  all  the  pages   of  the  web
    interface. *)
val hackojo_page : [ body_content_fun ] elt list -> html elt Lwt.t

(** [set_menu buttons] customizes the buttons in the menu bar. *)
val set_menu : [ div_content_fun ] elt list -> unit Lwt.t

(** [menu_button service label x] produces a button that applies [service]
    to [x] when clicked. *)
val menu_button :
  ?xa:Html5_types.a_attrib Eliom_content.Html5.D.attrib list ->
  ('a, unit, [< Eliom_service.get_service_kind ], [< Eliom_service.suff ],
   'b, unit, [< Eliom_service.registrable ], 'c)
    Eliom_service.service ->
  string ->
  'a -> [> [> `PCDATA ] Html5_types.a ] Eliom_content_core.Html5.elt

{shared{
val get_img :
  a:[< `Accesskey
       | `Class
       | `Contenteditable
       | `Contextmenu
       | `Dir
       | `Draggable
       | `Height
       | `Hidden
       | `Id
       | `Ismap
       | `OnAbort
       | `OnBlur
       | `OnCanPlay
       | `OnCanPlayThrough
       | `OnChange
       | `OnClick
       | `OnContextMenu
       | `OnDblClick
       | `OnDrag
       | `OnDragEnd
       | `OnDragEnter
       | `OnDragLeave
       | `OnDragOver
       | `OnDragStart
       | `OnDrop
       | `OnDurationChange
       | `OnEmptied
       | `OnEnded
       | `OnError
       | `OnFocus
       | `OnFormChange
       | `OnFormInput
       | `OnInput
       | `OnInvalid
       | `OnKeyDown
       | `OnKeyPress
       | `OnKeyUp
       | `OnLoad
       | `OnLoadStart
       | `OnLoadedData
       | `OnLoadedMetaData
       | `OnMouseDown
       | `OnMouseMove
       | `OnMouseOut
       | `OnMouseOver
       | `OnMouseUp
       | `OnMouseWheel
       | `OnPause
       | `OnPlay
       | `OnPlaying
       | `OnProgress
       | `OnRateChange
       | `OnReadyStateChange
       | `OnScroll
       | `OnSeeked
       | `OnSeeking
       | `OnSelect
       | `OnShow
       | `OnStalled
       | `OnSubmit
       | `OnSuspend
       | `OnTimeUpdate
       | `OnVolumeChange
       | `OnWaiting
       | `Spellcheck
       | `Style_Attr
       | `Tabindex
       | `Title
       | `User_data
       | `Width
       | `XML_lang
       | `XMLns ]
      Eliom_content.Html5.D.attrib list ->
    alt:Html5_types.text -> string -> [> `Img ] Eliom_content.Html5.D.elt
}}

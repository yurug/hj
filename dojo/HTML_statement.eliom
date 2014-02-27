(* -*- tuareg -*- *)

{shared{

open Lwt
open Eliom_content
open Html5.D
open Html5
open Html5_sigs
open CORE_statement

type any = Xml.elt

let weaken f = fun cs -> [f (List.map tot cs)]

let constructor_of_tag : tag -> any list -> 'a elt list = function
  | Paragraph -> weaken p
  | Sequence  -> List.map tot
  | Bold -> weaken (fun cs -> span ~a:[a_class ["bold"]] cs)
  | Italic -> weaken (fun cs -> span ~a:[a_class ["italic"]] cs)
  | List -> weaken ul
  | Enumerate -> weaken ol
  | Item -> weaken li
  | Section -> weaken h1
  | SubSection -> weaken h2
  | Question -> weaken h3
  | Text -> weaken span
  | Code -> weaken pre
  | Verb -> weaken Html5.D.code
  | LaTeX -> weaken (fun cs -> span (pcdata "\\[" :: cs @ [pcdata "\\]"]))
  | ILaTeX -> weaken (fun cs -> span (pcdata "\\(" :: cs @ [pcdata "\\)"]))
  | _ -> weaken pre

let rec to_html' = function
  | Element (Link, [Data url; Data caption]) ->
    [Raw.a ~a:[a_href (Xml.uri_of_string url)] [pcdata caption]]
  | Element (Image, [_; Data url; Data caption]) ->
    [Raw.img ~src:(Xml.uri_of_string url) ~alt:caption ()]
  | Element (tag, cs) ->
    constructor_of_tag tag (List.(flatten (map to_html cs)))
  | Data s ->
    [pcdata s]

and to_html d = List.map toelt (to_html' d)

let to_html d = List.map tot (to_html d)

}}

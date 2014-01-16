(* -*- tuareg -*- *)

{shared{

open Lwt
open Eliom_content
open Html5.D
open Html5
open Html5_sigs
open CORE_statement

type any = Xml.elt

let weaken f = fun cs -> f (List.map tot cs)

let constructor_of_tag : tag -> any list -> 'a elt = function
  | Paragraph -> weaken p
  | Sequence  -> weaken div
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
  | _ -> assert false

let rec to_html' = function
  | Element (Link, [Data url; Data caption]) ->
    Raw.a ~a:[a_href (Xml.uri_of_string url)] [pcdata caption]
  | Element (LaTeX, [Data text]) ->
    span [pcdata ("\\[" ^ text ^ "\\]")]
  | Element (ILaTeX, [Data text]) ->
    span [pcdata ("\\(" ^ text ^ "\\)")]
  | Element (tag, cs) ->
    constructor_of_tag tag (List.map to_html cs)
  | Data s ->
    pcdata s

and to_html d = toelt (to_html' d)

let to_html d = tot (to_html d)

}}

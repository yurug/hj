(* -*- tuareg -*- *)

{shared{

open Lwt
open Eliom_content
open Html5.D
open Html5
open Html5_sigs
open CORE_statement

type any = Xml.elt

let constructor_of_tag : tag -> any list -> any = function
  | Paragraph -> fun cs -> toelt (p (List.map tot cs))

let rec to_html = function
  | Element (tag, cs) ->
    constructor_of_tag tag (List.map to_html cs)
  | Data s ->
    toelt (pcdata s)

let to_html d = tot (to_html d)

}}

(* -*- tuareg -*- *)

{shared{
open Lwt
open Eliom_content.Html5
open Eliom_content.Html5.D
open Html5_types
open Eliom_service
}}

open Identifier
open Questions
open Statement
open WidgetHTML
open ExtPervasives

let string_template_as_html classes s =
  span ~a:[a_class classes] [pcdata (flatten_string s)]

let string_template_as_html_code classes s =
  code [pcdata (flatten_string s)]

let string_template_as_html_latex classes s =
  span [pcdata ("\\(" ^ flatten_string s ^ "\\)")]


let string_as_html classes s =
  span ~a:(if classes = [] then [] else [a_class classes]) [pcdata s]


let rec template_text_as_html classes t =
  List.rev (
    flatten []
      (fun a s -> string_as_html classes s :: a)
      (fun a s -> text_as_html classes s @ a) t
  )
and text_as_html classes = function
  | String s -> [string_template_as_html classes s]
  | Code s -> [string_template_as_html_code classes s]
  | LaTeX s -> [string_template_as_html_latex classes s]
  | Bold t -> template_text_as_html ("bold" :: classes) t
  | Italic t -> template_text_as_html ("italic" :: classes) t
  | RawHTML s ->
    let span = span [] in
    let s = flatten_string s in
    ignore ({unit{ (To_dom.of_element %span)##innerHTML <- Js.string %s }});
    [span]

  | RawLaTeX _ -> []
  | Hlink (url, caption) ->
    let url = flatten_string url
    and caption = string_template_as_html_code [] caption in
    [Raw.a ~a:[a_href (Xml.uri_of_string url)] [caption]]

let statement_as_html codes = function
  | Paragraph t ->
    [p (template_text_as_html [] t)]
  | Verbatim t ->
    [pre [pcdata (flatten_string t)]]
  | CodeBlock (l, t) ->
    let elt =
      pre [code ~a:[a_class [flatten_string l]] [
        pcdata (flatten_string t)
      ]]
    in
    codes := elt :: !codes;
    [elt]
  | RawHTMLBlock s ->
    let s = flatten_string s in
    let div = div [] in
    ignore ({unit{ (To_dom.of_element %div)##innerHTML <- Js.string %s }});
    [div]
  | RawLaTeXBlock s ->
    []


let statements_as_html codes t =
  List.rev (
    flatten []
      (fun a s -> p [string_as_html [] s] :: a)
      (fun a s -> statement_as_html codes s @ a)
      t
  )

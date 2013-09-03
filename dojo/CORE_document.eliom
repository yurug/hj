(* -*- tuareg -*- *)

{shared{

type _ document_type =
  | Text : string list document_type

type 'a document = {
  dtype   : 'a document_type;
  content : 'a;
}

type t = SomeDocument : 'a document -> t

let empty_text = SomeDocument {
  dtype = Text;
  content = []
}

let add_line d l =
  match d with
    | SomeDocument { dtype = Text; content } ->
      SomeDocument { dtype = Text; content = l :: content }

let lines : t -> string list = function
  | SomeDocument { dtype = Text; content } -> content

}}

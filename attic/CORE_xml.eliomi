(* -*- tuareg -*- *)

(** Low-level operations on XML documents. *)

(**  [plug path  host doc]  replaces the  node at  [path] by  [doc] in
    [host]. Raise  [InvalidPath (path, host)]  if there is no  node at
    [path] in [host]. *)
val plug :
  string list ->
  Simplexmlparser.xml -> Simplexmlparser.xml -> Simplexmlparser.xml

(** [InvalidXMLIO s] is raised instead of [Failure s] by
    the next two functions. *)
exception InvalidXMLIO of string

(** [load_xml fname] loads an untyped XML document from file [fname]. *)
val load_xml : string -> Simplexmlparser.xml

(** [save_xml fname doc] saves the untyped XML document [doc] from
    file [fname]. *)
val save_xml : string -> Simplexmlparser.xml -> unit

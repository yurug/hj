(* FIXME: Try not to reify [template] as it obfuscate the
   FIXME: client code. *)
type 'a template =
| TAtom of string * 'a template
| TCode of 'a * 'a template
| TNil
deriving (Json)

let flatten init on_string on_code s =
  let rec aux accu = function
    | TNil -> accu
    | TAtom (s, t) -> aux (on_string accu s) t
    | TCode (s, t) -> aux (on_code accu s) t
  in
  aux init s

let flatten_string s = flatten "" ( ^ ) ( ^ ) s

let cons xs x = x :: xs

let cons_skip_semicolon xs = function
  | ";" -> xs
  | x -> x :: xs

let flatten_list s =
  List.rev (
    flatten [] cons_skip_semicolon cons s
  )

type text =
  | Bold of text template
  | Italic of text template
  | String of string template
  | Code of string template
  | LaTeX of string template
  | Hlink of string template * string template
  | RawHTML of string template
  | RawLaTeX of string template
deriving (Json)

type statement =
| Paragraph of text template
| Verbatim  of string template
| CodeBlock of string template * string template
deriving (Json)

type string_t = string template
deriving (Json)

module ReifyFromAka = struct

  open AkaInterpreter
  open Name
  open XAST

  let lookup k fs = List.assoc (LName k) fs

  let boolean = function
    | VData (DName "True", []) -> true
    | VData (DName "False", []) -> false
    | _ -> assert false

  let string = function
    | VPrimitive (PStringConstant s) -> s
    | _ -> assert false

  let int = function
    | VPrimitive (PIntegerConstant x) -> x
    | _ -> assert false

  let rec template reify_code = function
    | VData (DName "TAtom", [ s; t ]) ->
      TAtom (string s, template reify_code t)
    | VData (DName "TCode", [ s; t ]) ->
      TCode (reify_code s, template reify_code t)
    | VData (DName "TNil", []) ->
      TNil
    | _ -> assert false

  let rec list reify_element = function
    | VData (DName "Cons", [ a; t ]) ->
      reify_element a :: list reify_element t
    | VData (DName "Nil", []) ->
      []
    | _ -> assert false

  let rec text = function
    | VData (DName "Bold", [ t ]) ->
      Bold (template text t)
    | VData (DName "Italic", [ t ]) ->
      Italic (template text t)
    | VData (DName "String", [ s ]) ->
      String (template string s)
    | VData (DName "Code", [ s ]) ->
      Code (template string s)
    | VData (DName "LaTeX", [ s ]) ->
      LaTeX (template string s)
    | VData (DName "Hlink", [ url; label]) ->
      Hlink (template string url, template string label)
    | VData (DName "RawHTML", [ s ]) ->
      RawHTML (template string s)
    | VData (DName "RawLaTeX", [ s ]) ->
      RawLaTeX (template string s)
    | _ -> assert false

  let statement = function
    | VData (DName "Paragraph", [t]) ->
      Paragraph (template text t)
    | VData (DName "Verbatim", [t]) ->
      Verbatim (template string t)
    | VData (DName "CodeBlock", [l; t]) ->
      CodeBlock (template string l, template string t)
    | _ -> assert false

end

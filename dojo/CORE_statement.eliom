(* -*- tuareg -*- *)

{shared{

type tag =
    Sequence | Paragraph | Bold | Italic | List
  | Enumerate | Item | Section | SubSection | Question
  | Text | Code | Link | LaTeX | ILaTeX
deriving (Json)

type t = Element of tag * t list | Data of string
deriving (Json)

}}

let make tag ts = Element (tag, ts)
let sequence = make Sequence
let paragraph = make Paragraph
let bold = make Bold
let italic = make Italic
let list = make List
let enumerate = make Enumerate
let item = make Item
let section = make Section
let subsection = make SubSection
let question = make Question
let code = make Code
let latex = make LaTeX
let ilatex = make ILaTeX
let text s = Data s
let link url caption = make Link [text url; text caption]

module LaTeX = struct

  (* FIXME: use a more efficient representation? *)
  (* FIXME: Move this to a general library for LaTeX generation. *)
  type latex = string list
  open List

  let all_escaped_characters =
    [
      (** The order matters. *)
      "\\\\", "\\textbackslash ";
      "{", "\\{";
      "}", "\\}";
      "_", "\\_";
      "\\$", "\\$";
      "%", "\\%";
      "#", "\\#";
      "&", "\\&";
      "~", "{\\textasciitilde}";
      "\\^", "{\\textasciicircum}";
    ]

  let verb_escaped_characters =
    [
      "_", "\\_";
    ]

  let escape_wrt cs s = Str.(
    List.fold_left
      (fun s (c, w) -> global_replace (regexp c) w s)
      s
  ) cs

  let escape = escape_wrt all_escaped_characters
  let escape_verb = escape_wrt verb_escaped_characters

  let text cs = String.concat " " (flatten cs)
  let paragraph cs = ["\\noindent"; text cs; "\n"; "\n"]
  let sequence cs = flatten cs
  let macro m cs = ["\\" ^ m ^ "{"; text cs; "}" ]
  let env e cs = ("\\begin{" ^ e ^ "}") :: flatten cs @ ["\\end{" ^ e ^ "}"]

  let bold = macro "textbf"
  let italic = macro "textit"
  let list = env "itemize"
  let enumerate = env "enumerate"
  let item = macro "item"
  let section = macro "section"
  let subsection = macro "subsection"
  let question = macro "subsubsection*"
  let code = env "code"

  let constructor_of_tag : tag -> latex list -> latex = function
    | Paragraph -> paragraph
    | Sequence -> sequence
    | Bold -> bold
    | Italic -> italic
    | List -> list
    | Enumerate -> enumerate
    | Item -> item
    | Section -> section
    | SubSection -> subsection
    | Question -> question
    | Text -> fun cs -> [text cs]
    | _ -> assert false

  let rec to_latex = function
    | Element (Code, [Data text]) ->
      code [env "verbatim" [[text]]]
    | Element (Link, [Data url; Data caption]) ->
      [Printf.sprintf "\\href{%s}{%s}" url (escape caption)]
    | Element (LaTeX, [Data text]) ->
      ["$$" ^ escape text ^ "$$"]
    | Element (ILaTeX, [Data text]) ->
      ["$" ^ escape text ^ "$"]
    | Element (tag, cs) ->
      constructor_of_tag tag (List.map to_latex cs)
    | Data s ->
      [escape s]

  let to_latex d = String.concat "\n" (to_latex d)

end

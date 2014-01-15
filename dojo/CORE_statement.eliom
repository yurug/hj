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

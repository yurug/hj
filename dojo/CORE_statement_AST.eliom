(** -*- tuareg -*- *)

(** A document format for exercise statements.

    Features:
    - hierarchical structures (title, sections, subsections, paragraphs,
      enumerations, lists).
    - font styles (bold, italic, ...).
    - image inclusion.
    - inline LaTeX (rendered by MathJax or by pdflatex).
    - inline HTML.

    A document can be rendered in PDF.
    A document can be composed of subdocuments.

*)

type t =
  | Structure of section_level * body * t list

and section_level =
  | Title
  | Section
  | Subsection
  | Paragraph
  | Enumeration
  | List
  | Item

and body = style * atom list

and atom =
  | Raw of string
  | Image of string
  | InlineLaTeX of string
  | InlineHTML of string

and style = style_modifier list

and style_modifier = Italic | Bold | Centered

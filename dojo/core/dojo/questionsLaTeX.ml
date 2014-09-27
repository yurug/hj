open Statement
open Questions
open Printf

let make q =
  let title = flatten_string q.etitle in
  let body =
    let buffer = Buffer.create 13 in
    let puts = Buffer.add_string buffer in

    let rec template f = function
      | TAtom (s, t) ->
        puts s;
        template f t
      | TCode (x, t) ->
        f x;
        template f t
      | TNil ->
        ()
    in

    let rec stars = function
      | 0 -> ""
      | n -> "$\\star$" ^ stars (n - 1)
    in

    let escape s = Str.(
      let s = global_replace (regexp "&") "\\&" s in
      s
    )
    in
    let flatten_string s = escape (flatten_string s) in

    let rec questions level = function
      | Section (title, q) ->
        begin match level with
          | 1 -> puts (sprintf "\\section{%s}\n" (flatten_string title))
          | 2 -> puts (sprintf "\\subsection{%s}\n" (flatten_string title))
          | 3 -> puts (sprintf "\\subsubsection{%s}\n" (flatten_string title))
          | _ -> puts (sprintf "\\paragraph{%s}\n" (flatten_string title))
        end;
        template (questions (level + 1)) q

      | Question q ->
        puts (sprintf "\\paragraph{\\textbf{%s}} (%s) \
                       {\\hfill\\scriptsize %s [%s]}\n\n"
                (flatten_string q.title)
                (stars q.difficulty)
                (String.concat " " q.tags)
                (flatten_string q.id));
        template statement q.statement;
        template context q.context

    and context = function
      | QCM (choices, _) ->
        puts "\\begin{itemize}\n";
        List.iter (fun t ->
          puts "\\item[$\\square$]";
          template text t
        ) choices;
        puts "\\end{itemize}\n\n"

      | Grader (expected_file, _, _) ->
        puts (sprintf "\\hfill$\\triangleright$ \\verb!%s!"
                expected_file)

      | WITV (values, _, _) ->
        puts "\\begin{tabular}{|l|r|}\n";
        puts "\\hline\n";
        List.iter (fun t ->
          template text t;
          puts "& \\hspace{4cm} \\\\"
        ) values;
        puts "\\hline\n";
        puts "\\end{tabular}\n\n"

      | Chooser _ ->
        ()

      | NoGrade ->
        ()

    and statement = function
      | Paragraph t ->
        template text t;
        puts "\n\n"
      | Verbatim t ->
        puts "\\begin{code}\n";
        puts "\\begin{verbatim}\n";
        puts (flatten_string t);
        puts "\\end{verbatim}\n";
        puts "\\end{code}\n\n"
      | CodeBlock (language, code) ->
        puts "\\begin{code}\n";
        puts (sprintf "\\begin{lstlisting}[language=%s]\n"
                (flatten_string language));
        puts (flatten_string code);
        puts "\\end{lstlisting}\n";
        puts "\\end{code}\n\n";
      | RawHTMLBlock _ ->
        ()
      | RawLaTeXBlock s ->
        puts (flatten_string s)

    and text = function
      | Bold t ->
        puts "\\textbf{";
        template text t;
        puts "}"
      | Italic t ->
        puts "\\textit{";
        template text t;
        puts "}"
      | String s ->
        puts (flatten_string s)
      | Code t ->
        puts (sprintf "\\verb!%s!" (flatten_string t))
      | LaTeX t ->
        puts (sprintf "$%s$" (flatten_string t))
      | RawHTML s ->
        ()
      | RawLaTeX s ->
        puts (flatten_string s)
      | Hlink (url, caption) ->
        puts (sprintf "\\url{%s}{%s}"
                (flatten_string url)
                (flatten_string caption))

    in
    template (questions 1) q.questions;
    Buffer.contents buffer
  in
  Printf.sprintf "
        \\documentclass{article}
        \\usepackage{hyperref}
        \\usepackage[french]{babel}
        \\usepackage[utf8]{inputenc}
        \\usepackage[T1]{fontenc}
        \\usepackage{amssymb}
        \\usepackage{fancybox}
        \\usepackage{fullpage}
        \\usepackage{graphics}
        \\usepackage{listings}
        \\usepackage{textcomp}
        \\title{%s}
        \\date{}
        \\usepackage{eurosym}
        \\DeclareUnicodeCharacter{20AC}{\\euro}

        \\hypersetup{
            bookmarks=true,
            unicode=false,
            pdftoolbar=true,
            pdfmenubar=true,
            pdffitwindow=false,
            pdfstartview={FitH},
            pdfnewwindow=true,
            colorlinks=yes,
            linkcolor=blue,
            citecolor=green,
            filecolor=magenta,
            urlcolor=cyan
        }
       \\newenvironment{code}{
        \\begin{center}
        \\Sbox
        \\hspace{0.3cm}\\minipage{14.7cm}
        }{
        \\endminipage
        \\endSbox\\fbox{\\TheSbox}
        \\end{center}
        }
        \\DeclareUnicodeCharacter{00A0}{ }
        \\begin{document}
        \\maketitle
         %s
        \\end{document}
      "
    title body

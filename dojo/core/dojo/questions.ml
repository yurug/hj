(** The following type definitions mimick the ones found in [std.aka]. *)

type 'a template =
| TAtom of string * 'a template
| TCode of 'a * 'a template
| TNil

let flatten init on_string on_code =
  let rec aux accu = function
    | TNil -> accu
    | TAtom (s, t) -> aux (on_string accu s) t
    | TCode (s, t) -> aux (on_code accu s) t
  in
  aux init

type text = {
  bold   : bool;
  italic : bool;
  value  : string
}

type statement =
| Text of text template

type context =
| QCM of statement list * int list

type string_t = string template

type questions =
| Question of string_t * string_t * statement template * context template
| Section  of string_t * questions template


(* FIXME: The following code could be generated from std.aka to improve
   FIXME: robustness. *)
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

  let text = function
    | VRecord fields ->
      {
        bold   = boolean (lookup "bold" fields);
        italic = boolean (lookup "italic" fields);
        value  = string (lookup "value" fields)
      }
    | _ -> assert false

  let statement = function
    | VData (DName "Text", [t]) ->
      Text (template text t)
    | _ -> assert false

  let context = function
    | VData (DName "QCM", [choices; expected_choices]) ->
      QCM (list statement choices, list int expected_choices)
    | _ -> assert false

  let rec questions = function
    | VData (DName "Question", [n; t; s; c]) ->
      Question (template string n, template string t,
                template statement s, template context c)
    | VData (DName "Section", [n; q]) ->
      Section (template string n, template questions q)
    | _ -> assert false

end

module Txt = struct

  open Printf

  let string_template = flatten "" ( ^ ) ( ^ )

  let vcat f = flatten "" ( ^ ) (fun s c -> s ^ "\n" ^ f c)

  let text t = t.value

  let statement = function
    | Text s -> flatten "" ( ^ ) (fun s t -> s ^ text t) s

  let context = function
    | QCM (choices, _) ->
      String.concat "\n" (List.(
        mapi (fun i -> (sprintf "%d. %s") (i + 1)) (map statement choices)
      ))

  let rec questions level = function
    | Question (id, title, s, c) ->
      Printf.sprintf "[%s] %s\n%s\n%s\n"
        (string_template id)
        (string_template title)
        (vcat statement s)
        (vcat context c)

    | Section (title, q) ->
      Printf.sprintf "%s %s\n%s\n"
        (String.make level '*')
        (string_template title)
        (vcat (questions (level + 1)) q)

  let questions = questions 1

end

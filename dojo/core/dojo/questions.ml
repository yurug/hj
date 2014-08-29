(** The following type definitions mimick the ones found in [std.aka]. *)

open ExtPervasives

type 'a template =
| TAtom of string * 'a template
| TCode of 'a * 'a template
| TNil
deriving (Json)

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
deriving (Json)

type statement =
| Text of text template
deriving (Json)

type context =
| QCM of statement list * int list
deriving (Json)

type string_t = string template
deriving (Json)

type questions =
| Question of string_t * string_t * statement template * context template
| Section  of string_t * questions template
deriving (Json)

type t = questions deriving (Json)

type identifier = string deriving (Json)

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

type answer =
  | Invalid
  | Choices of int list
deriving (Json)

type answers = (identifier * answer) list
deriving (Json)

let empty_answers = []

let new_answer answers qid answer =
  update_assoc qid answer answers

let string_of_answer = function
  | Invalid -> "invalid answer"
  | Choices cs ->
    Printf.sprintf "choices %s"
      (String.concat ", " (List.map string_of_int cs))

let answer_to_string = function
  | Invalid -> "invalid_answer"
  | Choices cs ->
    Printf.sprintf "choose:%s"
      (String.concat "," (List.map string_of_int cs))

let string_to_answer s = Str.(
  try
    begin match split (regexp ":") s with
      | [ "choose" ; cs ] ->
        let cs = split (regexp "[, \t]+") cs in
        let cs = List.filter (fun x -> x <> "") cs in
        Choices (List.map int_of_string cs)
      | _ ->
        Invalid
    end
  with _ -> Invalid
)

let answer_to_json s =
  Yojson.Safe.from_string (Deriving_Json.to_string Json.t<answer> s)

type score = int * int deriving (Json)

type trace_event =
  | Message of string
deriving (Json)

type trace = trace_event list
deriving (Json)

type criteria =
  | Automatic
  | UserDefined of string
deriving (Json)

type grade = {
  scores : (criteria * score) list;
  trace  : trace
}
deriving (Json)

let string_of_criteria = function
  | Automatic -> "automatic"
  | UserDefined s -> "user_" ^ s

let string_of_score (c, (m, o)) =
  Printf.sprintf "%s:%d/%d"
    (string_of_criteria c)
    m
    o

let string_of_scores ss =
  String.concat ", " (List.map string_of_score ss)

let string_of_trace_event = function
  | Message s -> s

let string_of_trace t =
  String.concat "\n" (List.map string_of_trace_event t)

let string_of_grade grade =
  Printf.sprintf "%s\nTrace:\n%s"
    (string_of_scores grade.scores)
    (string_of_trace grade.trace)

type evaluation_job_identifier = int deriving (Json)

type evaluation_error =
  | UnboundQuestion
  | SyntaxErrorInAnswer
  | InvalidContextDescription
  | NoAnswer
deriving (Json)

type evaluation_state =
  | EvaluationError of evaluation_error
  | EvaluationDone of grade
  | EvaluationWaits
  | EvaluationHandled of evaluation_job_identifier
deriving (Json)

type evaluations =
  (identifier * evaluation_state) list
deriving (Json)

let empty_evaluations = []

let matched_question_identifier qid id =
  flatten "" ( ^ ) ( ^ ) id = qid

let lookup_question qs qid =
  let rec questions = function
  | Question (id, title, statement, context) ->
    if matched_question_identifier qid id then
      Some (title, statement, context)
    else
      None
  | Section (_, qs) ->
    find_question qs

  and find_question = function
    | TNil -> None
    | TAtom (_, qs) -> find_question qs
    | TCode (q, qs) ->
      match questions q with
        | None -> find_question qs
        | Some q -> Some q
  in
  questions qs

type context_descriptor =
  | CtxQCM of int list

let evaluate_using_context context answer =
  match context, answer with
    | CtxQCM expected_choices, Choices cs ->
      let automatic_score =
        if List.(sort compare expected_choices = sort compare cs) then
          (1, 1)
        else
          (0, 1)
      in
      EvaluationDone {
        scores = [ (Automatic, automatic_score) ];
        trace  = []
      }
    | _, Invalid ->
      EvaluationError SyntaxErrorInAnswer

let make_context ctx_makers =
  let rec aux ctx ctx_maker =
    match ctx, ctx_maker with
    | None, QCM (_, xchoices) ->
      Some (CtxQCM xchoices)
    | Some _, QCM _ ->
      None
  in
  flatten None (fun accu _ -> accu) aux ctx_makers

let evaluate questions qid answer =
  match lookup_question questions qid with
    | Some (title, statement, context_makers) ->
      begin match make_context context_makers with
        | None -> EvaluationError InvalidContextDescription
        | Some context -> evaluate_using_context context answer
      end
    | None ->
      EvaluationError UnboundQuestion

let update_evaluations evaluations questions qid answer =
  update_assoc qid (evaluate questions qid answer) evaluations

let evaluation_state evaluations qid =
  try
    List.assoc qid evaluations
  with Not_found ->
    EvaluationError NoAnswer

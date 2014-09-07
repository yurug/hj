(** The following type definitions mimick the ones found in [std.aka]. *)

open Lwt
open ExtPervasives

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
deriving (Json)

type statement =
| Paragraph of text template
| Verbatim  of string template
| CodeBlock of string template * string template
deriving (Json)

type context =
| QCM of text template list * int list
| Grader of string * string list * string
deriving (Json)

type string_t = string template
deriving (Json)

type question = {
  id         : string_t;
  title      : string_t;
  tags       : string list;
  difficulty : int;
  statement  : statement template;
  context    : context template;
}
deriving (Json)

type questions =
| Question of question
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
    | _ -> assert false

  let statement = function
    | VData (DName "Paragraph", [t]) ->
      Paragraph (template text t)
    | VData (DName "Verbatim", [t]) ->
      Verbatim (template string t)
    | VData (DName "CodeBlock", [l; t]) ->
      CodeBlock (template string l, template string t)
    | _ -> assert false

  let word s =
    Str.(global_replace (regexp " ") "" s)

  let words l =
    List.(filter (fun x -> x <> "") (map word (flatten_list l)))

  let context = function
    | VData (DName "QCM", [choices; expected_choices]) ->
      QCM (list (template text) choices, list int expected_choices)
    | VData (DName "Grader", [expected_file; import_files; command]) ->
      let expected = word (flatten_string (template string expected_file)) in
      let import_files = words (template string import_files) in
      let command = flatten_string (template string command) in
      Grader (expected, import_files, command)
    | _ -> assert false

  let question = function
    | VRecord q ->
      {
        id         = template string (lookup "id" q);
        title      = template string (lookup "title" q);
        tags       = words (template string (lookup "tags" q));
        difficulty = int (lookup "difficulty" q);
        statement  = template statement (lookup "statement" q);
        context    = template context (lookup "context" q);
      }
    | _ -> assert false


  let rec questions = function
    | VData (DName "Question", [q]) ->
      Question (question q)
    | VData (DName "Section", [n; q]) ->
      Section (template string n, template questions q)
    | _ -> assert false

end

module Txt = struct

  open Printf

  let vcat f = flatten "" ( ^ ) (fun s c -> s ^ "\n" ^ f c)

  let rec flatten_text t = flatten "" ( ^ ) (fun s t -> s ^ text t) t

  and text = function
    | Bold t | Italic t -> flatten_text t
    | String s -> flatten_string s
    | Code s -> "[" ^ flatten_string s ^ "]"
    | LaTeX s -> "$" ^ flatten_string s ^ "$"

  let paragraph t = text t

  let string x = x

  let statement = function
    | Paragraph s ->
      vcat text s
    | CodeBlock (l, s) ->
      "codeblock [" ^ flatten_string l ^ "] {\n" ^ vcat string s ^ "\n}"
    | Verbatim  s ->
      "verbatim  {\n" ^ vcat string s ^ "\n}"

  let context = function
    | QCM (choices, _) ->
      String.concat "\n" (List.(
        mapi
          (fun i -> (sprintf "%d. %s") (i + 1))
          (map flatten_text choices)
      ))
    | Grader (expected_file, _, _) ->
      sprintf "%s?" expected_file

  let rec questions level = function
    | Question q ->
      Printf.sprintf "[%s] %s [%s / %d]\n%s\n%s\n"
        (flatten_string q.id)
        (flatten_string q.title)
        (String.concat ", " q.tags)
        q.difficulty
        (vcat statement q.statement)
        (vcat context q.context)

    | Section (title, q) ->
      Printf.sprintf "%s %s\n%s\n"
        (String.make level '*')
        (flatten_string title)
        (vcat (questions (level + 1)) q)

  let questions = questions 1

end

type answer =
  | Invalid
  | Choices of int list
  | File of string
deriving (Json)

type answers = (identifier * answer) list
deriving (Json)

let empty_answers = []

let new_answer answers qid answer =
  update_assoc qid answer answers

let lookup_answer answers qid =
  List.assoc qid answers

let iter_answers_s = Lwt_list.iter_s

let string_of_answer = function
  | Invalid -> "invalid answer"
  | Choices cs ->
    Printf.sprintf "choices %s"
      (String.concat ", " (List.map string_of_int cs))
  | File s ->
    Printf.sprintf "file %s" s

let answer_to_string = function
  | Invalid -> "invalid_answer"
  | Choices cs ->
    Printf.sprintf "choose:%s"
      (String.concat "," (List.map string_of_int cs))
  | File f ->
    Printf.sprintf "file:%s" f

let string_to_answer s = Str.(
  try
    begin match split (regexp ":") s with
      | [ "choose" ; cs ] ->
        let cs = split (regexp "[, \t]+") cs in
        let cs = List.filter (fun x -> x <> "") cs in
        Choices (List.map int_of_string cs)
      | [ "file"; filename ] ->
        File (filename)
      | _ ->
        Invalid
    end
  with _ -> Invalid
)

let answer_to_json s =
  Yojson.Safe.from_string (Deriving_Json.to_string Json.t<answer> s)

type score = int * int deriving (Json)

(* FIXME:
   We should also have:
   - teacher-only messages ;
   - widget directed message
*)
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
  | IncompatibleAnswer
  | NoAnswer
  | ErrorDuringGraderExecution
deriving (Json)

type evaluation_state =
  | EvaluationError of evaluation_error
  | EvaluationDone of identifier * string list * int * grade
  | EvaluationWaits
  | EvaluationHandled of evaluation_job_identifier
deriving (Json)

type evaluations =
  (identifier * evaluation_state) list
deriving (Json)

let string_of_evaluation_state = function
  | EvaluationError _ -> "error"
  | EvaluationDone (qid, _, _, g) -> qid ^ ":" ^ string_of_grade g
  | EvaluationWaits -> "waiting"
  | EvaluationHandled _ -> "handled"

let empty_evaluations = []

let lookup_evaluation_state qid evaluations =
  try
    Some (List.assoc qid evaluations)
  with Not_found ->
    None

let matched_question_identifier qid id =
  flatten "" ( ^ ) ( ^ ) id = qid

let lookup_question qs qid =
  let rec questions = function
  | Question q ->
    if matched_question_identifier qid q.id then
      Some (q.title, q.tags, q.difficulty, q.statement, q.context)
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
  | CtxGrader of string * string list * string

let grade_qcm qid tags difficulty expected_choices cs =
  let automatic_score =
    if List.(sort compare expected_choices = sort compare cs) then
      (1, 1)
    else
      (0, 1)
  in
  EvaluationDone (qid, tags, difficulty, {
    scores = [ (Automatic, automatic_score) ];
    trace  = []
  })

let grade_regexp = Str.regexp "GRADE \\([0-9]+\\) \\([0-9]+\\)/\\([0-9]+\\)"

let grade_program qid tags difficulty files cmd update =
  let trace                     = ref [] in
  let automatic_score           = ref 0 in
  let automatic_potential_score = ref 0 in

  let puts s = return (trace := Message s :: !trace) in
  let putline s = puts (s ^ "\n") in

  let process_stdout s = puts s in

  (* Generate a secret seed. *)
  let secret = Seed.generate () in

  let process_stderr s = Str.(
    if string_match grade_regexp s 0 then
      let seed = Seed.of_string (matched_group 1 s) in
      if Seed.compare seed secret = 0 then return (
        automatic_score := int_of_string (matched_group 2 s);
        automatic_potential_score := int_of_string (matched_group 3 s)
      ) else
        return ()
    else
      (* FIXME: put this on the teacher only side. *)
      return ()
  )
  in
  let observer = Sandbox.(function
    | WaitingForSandbox _ -> putline "Waiting..."
    | FileModification _ -> return ()
    | WriteStdout (_, s) -> process_stdout s
    | WriteStderr (_, s) -> process_stderr s
    | Exited s ->
    (* Generate score and trace. *)
      let scores =
        [ (Automatic, (!automatic_score, !automatic_potential_score)) ]
      in
      let trace = List.rev !trace in
      update (EvaluationDone (qid, tags, difficulty, { scores; trace }))
  ) in
  (* Run the command. *)
  let cmd = Str.(global_replace (regexp "%seed") (Seed.to_string secret) cmd) in
  Printf.eprintf "Files: %s\n" (String.concat ", " files);
  Sandbox.(exec files cmd observer)
  >>= function
    | `OK (job, persistence_id) ->
      return (EvaluationHandled job)
    | `KO e ->
      (* FIXME: Provide a more detailed diagnostic. *)
      return (EvaluationError ErrorDuringGraderExecution)

let evaluate_using_context
    qid tags difficulty
    exo_real_path answer_real_path context answer update
=
  match context, answer with
    | CtxQCM expected_choices, Choices cs ->
      return (grade_qcm qid tags difficulty expected_choices cs)
    | CtxGrader (expected_file, imported_files, command), File filename ->
      let files =
        answer_real_path expected_file
        :: List.map exo_real_path imported_files
      in
      (* FIXME: Check that filename' = expected_file. *)
      grade_program qid tags difficulty files command update
    | _, Invalid ->
      return (EvaluationError SyntaxErrorInAnswer)
    | _, _ ->
      return (EvaluationError IncompatibleAnswer)

let make_context ctx_makers =
  let rec aux ctx ctx_maker =
    match ctx, ctx_maker with
    | None, QCM (_, xchoices) ->
      Some (CtxQCM xchoices)
    | None, Grader (expected_file, imported_files, command) ->
      Some (CtxGrader (expected_file, imported_files, command))
    | Some _, (QCM _ | Grader _) ->
      None
  in
  flatten None (fun accu _ -> accu) aux ctx_makers

let evaluate exo_real_path answer_real_path questions qid answer update =
  match lookup_question questions qid with
    | Some (title, tags, difficulty, statement, context_makers) ->
      begin match make_context context_makers with
        | None ->
          return (EvaluationError InvalidContextDescription)
        | Some context ->
          evaluate_using_context
            qid tags difficulty
            exo_real_path answer_real_path context answer update
      end
    | None ->
      return (EvaluationError UnboundQuestion)

let update_evaluation qid evaluation_state evaluations =
  (* Cancel existing job if needed. *)
  begin match lookup_evaluation_state qid evaluations, evaluation_state with
    | Some (EvaluationHandled job'), EvaluationHandled job when job <> job' ->
      Sandbox.cancel job'
    | _, _ ->
      ()
  end;
  update_assoc qid evaluation_state evaluations

let update_evaluations
    exo_real_path answer_real_path evaluations questions qid answer update
=
  lwt state =
    evaluate exo_real_path answer_real_path questions qid answer update
  in
  return (update_evaluation qid state evaluations, state)

let evaluation_state evaluations qid =
  try
    List.assoc qid evaluations
  with Not_found ->
    EvaluationError NoAnswer

let full_grade g =
  List.for_all (fun (_, (i, o)) -> i = o) g.scores

let on_completed evaluation_state f =
  match evaluation_state with
    | EvaluationDone (who, tags, difficulty, grade) ->
      if full_grade grade then
        f who tags difficulty
      else
        return ()
    | _ -> return ()

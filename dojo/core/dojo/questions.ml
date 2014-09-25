(** The following type definitions mimick the ones found in [std.aka]. *)

open Lwt
open ExtPervasives
open Statement

type context =
| QCM of text template list * int list
| Grader of string * string list * string
| WITV of text template list * string list * string
| Chooser of string list
| NoGrade
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

type exercise = {
  etitle : string_t;
  group_tags : string list;
  questions : questions template
}
deriving (Json)

type t = exercise deriving (Json)

let empty_exercise = {
  etitle = TNil;
  group_tags = [];
  questions = TNil;
}

type identifier = string deriving (Json)

let word s =
  Str.(global_replace (regexp " \\|\n\\|\t") "" s)

let words l =
  List.(filter (fun x -> x <> "") (map word (flatten_list l)))

(* FIXME: The following code could be generated from std.aka to improve
   FIXME: robustness. *)
module ReifyFromAka = struct

  open AkaInterpreter
  open Name
  open XAST
  open Statement.ReifyFromAka

  let context = function
    | VData (DName "QCM", [choices; expected_choices]) ->
      QCM (list (template text) choices, list int expected_choices)
    | VData (DName "Grader", [expected_file; import_files; command]) ->
      let expected = word (flatten_string (template string expected_file)) in
      let import_files = words (template string import_files) in
      let command = flatten_string (template string command) in
      Grader (expected, import_files, command)
    | VData (DName "WITV", [ expressions; values; comparator ]) ->
      WITV (list (template text) expressions,
            list string values,
            flatten_string (template string comparator))
    | VData (DName "Chooser", [ choices ]) ->
      Chooser (list string choices)
    | VData (DName "NoGrade", [])->
      NoGrade
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

  let exercise = function
    | VRecord e ->
      {
        etitle     = template string (lookup "etitle" e);
        group_tags = words (template string (lookup "group_tags" e));
        questions  = template questions (lookup "questions" e)
      }
    | _ -> assert false

end

module Txt = struct

  open Printf

  let vcat f = flatten "" ( ^ ) (fun s c -> s ^ "\n" ^ f c)

  let rec flatten_text t = flatten "" ( ^ ) (fun s t -> s ^ text t) t

  and text = function
    | Bold t | Italic t ->
      flatten_text t
    | String s ->
      flatten_string s
    | Code s ->
      "[" ^ flatten_string s ^ "]"
    | LaTeX s ->
      "$" ^ flatten_string s ^ "$"
    | RawHTML s ->
      "\n<raw_html>\n" ^ flatten_string s ^ "\n</raw_html>\n"
    | RawLaTeX s ->
      "\n<raw_latex>\n" ^ flatten_string s ^ "\n</raw_latex>\n"
    | Hlink (url, caption) ->
      "[" ^ flatten_string url ^ " | " ^ flatten_string caption ^ "]"

  let paragraph t = text t

  let string x = x

  let statement = function
    | Paragraph s ->
      vcat text s
    | CodeBlock (l, s) ->
      "codeblock [" ^ flatten_string l ^ "] {\n" ^ vcat string s ^ "\n}"
    | Verbatim  s ->
      "verbatim  {\n" ^ vcat string s ^ "\n}"
    | RawHTMLBlock s ->
      "raw_html_block [" ^ flatten_string s ^ "]"
    | RawLaTeXBlock s ->
      "raw_latex_block [" ^ flatten_string s ^ "]"

  let context = function
    | QCM (choices, _) ->
      String.concat "\n" (List.(
        mapi
          (fun i -> (sprintf "%d. %s") (i + 1))
          (map flatten_text choices)
      ))
    | Grader (expected_file, _, _) ->
      sprintf "%s?" expected_file

    | WITV (expressions, _, _) ->
      String.concat "\n" (List.(
        map
          (fun x -> x)
          (map flatten_text expressions)
      ))

    | Chooser (choices) ->
      String.concat "\n" (List.(
        mapi
          (fun i -> (sprintf "%d. %s") (i + 1))
          (map string choices)
      ))

    | NoGrade ->
      ""

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

  let exercise e =
    Printf.sprintf "%s [%s]\n%s\n"
      (flatten_string e.etitle)
      (String.concat ", " e.group_tags)
      (vcat questions e.questions)

end

type answer =
  | Invalid
  | Choices of int list
  | Choice of int
  | File of string
  | GivenValues of string list
deriving (Json)

module AnswersStore = Rb.Dict (struct
  type key = identifier (* Question identifier *)
  deriving (Json)

  type image = answer * Identifier.identifier (* User identifier *)
  deriving (Json)

  let compare = String.compare
end)

type answers = AnswersStore.t
deriving (Json)

let empty_answers = AnswersStore.empty

let new_answer answers qid answer author =
  AnswersStore.update qid (answer, author) answers

let lookup_answer answers qid =
  AnswersStore.lookup qid answers

let iter_answers_s f s = AnswersStore.lwt_iter s f

let string_of_answer = function
  | Invalid -> "invalid answer"
  | Choices cs ->
    Printf.sprintf "choices %s"
      (String.concat ", " (List.map string_of_int cs))
  | Choice c ->
    Printf.sprintf "choice %d" c
  | File s ->
    Printf.sprintf "file %s" s
  | GivenValues vs ->
    Printf.sprintf "given_values %s"
      (String.concat ", " vs)

let answer_to_string = function
  | Invalid -> "invalid_answer"
  | Choices cs ->
    Printf.sprintf "choices:%s"
      (String.concat "," (List.map string_of_int cs))
  | Choice c ->
    Printf.sprintf "choice:%d" c
  | File f ->
    Printf.sprintf "file:%s" f
  | GivenValues vs ->
    Printf.sprintf "given:%s" (String.concat "," vs)

let string_to_answer s = Str.(
  try
    begin match split (regexp ":") s with
      | [ "choices" ; cs ] ->
        let cs = split (regexp "[, \t]+") cs in
        let cs = List.filter (fun x -> x <> "") cs in
        Choices (List.map int_of_string cs)
      | [ "choice" ; c ] ->
        Choice (int_of_string (word c))
      | [ "file"; filename ] ->
        File (filename)
      | ["given"; vs ] ->
        let vs = split (regexp "[, \t]+") vs in
        let vs = List.filter (fun x -> x <> "") vs in
        GivenValues vs
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
  scores   : (criteria * score) list;
  trace    : trace;
  commands : string list
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

let small_string_of_grade grade =
  string_of_scores grade.scores

let string_of_grade_trace grade =
  string_of_trace grade.trace

type evaluation_job_identifier = int deriving (Json)

type evaluation_error =
  | UnboundQuestion of identifier
  | SyntaxErrorInAnswer
  | InvalidContextDescription
  | IncompatibleAnswer
  | NoAnswer
  | ErrorDuringGraderExecution
deriving (Json)

let string_of_evaluation_error = function
  | UnboundQuestion id -> "unbound_question_(" ^ id ^ ")"
  | SyntaxErrorInAnswer -> "syntax_error_in_answer"
  | InvalidContextDescription -> "invalid_context_description"
  | NoAnswer -> "no_answer"
  | ErrorDuringGraderExecution -> "error_during_grader_execution"
  | IncompatibleAnswer -> "incompatible_answer"

type evaluation_state =
  | EvaluationError of evaluation_error
  | EvaluationDone of identifier * string list * int * grade
  | EvaluationWaits
  | EvaluationHandled of evaluation_job_identifier
deriving (Json)

let is_completed = function
  | EvaluationDone (_, _, _, g) ->
    List.for_all (fun (_, (i, o)) -> i = o) g.scores
  | _ ->
    false

type evaluations =
  (identifier * evaluation_state) list
deriving (Json)

let evaluations_iter e f =
  List.iter (fun (q, s) -> f q s) e

let string_of_evaluation_state = function
  | EvaluationError e -> Printf.sprintf "%s" (string_of_evaluation_error e)
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
  | CtxWITV of string list * string
  | CtxChooser of string list
  | CtxNoGrade

let grade_qcm qid tags difficulty expected_choices cs =
  let automatic_score =
    if List.(sort compare expected_choices = sort compare cs) then
      (1, 1)
    else
      (0, 1)
  in
  EvaluationDone (qid, tags, difficulty, {
    scores = [ (Automatic, automatic_score) ];
    trace  = [];
    commands = []
  })

let grade_regexp = Str.regexp "GRADE \\([0-9]+\\) \\([0-9]+\\)/\\([0-9]+\\)"

let command_regexp = Str.regexp "COMMAND \\([0-9]+\\) \\(.*\\)$"

let grade_program qid tags difficulty files cmd update =
  let trace_size                = ref 0 in
  let trace                     = ref [] in
  let automatic_score           = ref 0 in
  let automatic_potential_score = ref 0 in
  let commands                  = ref [] in

  let puts s =
    incr trace_size;
    if !trace_size < 1024 then
      return (trace := Message s :: !trace)
    else if !trace_size = 1024 then
      return (trace := Message "-- too long trace --" :: !trace)
    else
      return ()
  in
  let putline s = puts (s ^ "\n") in

  let process_stdout s = puts s in

  let debug = Log.debug (Identifier.identifier_of_string "questions") in

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
    else if string_match command_regexp s 0 then
      let seed = Seed.of_string (matched_group 1 s) in
      if Seed.compare seed secret = 0 then return (
        commands := matched_group 2 s :: !commands
      ) else
        return ()
    else (
      debug s;
      (* FIXME: put this on the teacher only side. *)
      return ()
    )
  )
  in

  (* Run the command. *)
  let cmd = Str.(global_replace (regexp "%seed") (Seed.to_string secret) cmd) in

  let string_of_status = function
    | Unix.WEXITED d -> Printf.sprintf "exited(%d)" d
    | Unix.WSIGNALED d -> Printf.sprintf "interrupted(%d)" d
    | Unix.WSTOPPED d -> Printf.sprintf "stopped(%d)" d
  in
  let observer = Sandbox.(function
    | WaitingForSandbox _ -> putline "Waiting..."
    | FileModification _ -> return ()
    | WriteStdout (_, s) -> process_stdout s
    | WriteStderr (_, s) -> process_stderr s
    | Exited s ->
      debug (Printf.sprintf "[%s] %s\n" (string_of_status s) cmd);
      (* Generate score and trace. *)
      let scores =
        [ (Automatic, (!automatic_score, !automatic_potential_score)) ]
      in
      let trace = List.rev !trace in
      let commands = List.rev !commands in
      update (EvaluationDone (qid, tags, difficulty,
                              { scores; trace; commands }))
  ) in
  Sandbox.(exec files cmd observer ~limitations:[TimeOut 180.])
  >>= function
    | `OK (job, persistence_id) ->
      return (EvaluationHandled job)
    | `KO e ->
      (* FIXME: Provide a more detailed diagnostic. *)
      return (EvaluationError ErrorDuringGraderExecution)

let import_files exo_real_path filenames =
  let externs, locals =
    List.partition (fun f -> String.length f >= 1 && f.[0] = '#') filenames
  in
  let resolve filename =
    let len = String.length filename in
    let path = String.sub filename 1 (len - 1) in
    let id = Identifier.identifier_of_string (Filename.dirname path) in
    let basename = Filename.basename path in
    OnDisk.resource_real_path id basename
  in
  List.map exo_real_path locals
  @ List.map resolve externs

let grade_witv
    exo_real_path
    qid tags difficulty comparator_script
    expected_values values update
= List.(
  let trace = [] in
  let commands = [] in

  let failure () =
    let scores = [ (Automatic, (0, length expected_values)) ] in
    EvaluationDone (qid, tags, difficulty, { scores; trace; commands })
  in
  if length expected_values <> length values then
    return (failure ())
  else (
    let score = ref 0 in
    let normalize s = Str.(
      global_replace (regexp " \\|\t") "" s
    )
    in
    let check_value xvalue value =
      if normalize value = normalize xvalue then incr score
    in
    List.iter2 check_value expected_values values;
    let scores = [ (Automatic, (!score, length expected_values)) ] in
    return (EvaluationDone (qid, tags, difficulty, { scores; trace; commands }))
  ))

let make_choice qid choices x =
  let choice = List.nth choices x in
  let scores = [ (Automatic, (1, 1)) ] in
  let trace = [] in
  let others = List.filter (( <> ) choice) choices in
  let tags = choice :: List.map Tag.negate others  in
  let commands = [] in
  return (EvaluationDone (qid, tags, 1, { scores; trace; commands }))

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
        :: import_files exo_real_path imported_files
      in
      (* FIXME: Check that filename' = expected_file. *)
      grade_program qid tags difficulty files command update
    | CtxWITV (expected_values, comparator), GivenValues vs ->
      grade_witv
        exo_real_path
        qid tags difficulty
        comparator expected_values vs update
    | CtxChooser choices, Choice x ->
      make_choice qid choices x
    | CtxNoGrade, _ ->
      let scores = [] and trace = [] and commands = [] in
      return (EvaluationDone (qid, tags, 0, { scores; trace; commands }))
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
    | None, WITV (_, xvalues, comparator) ->
      Some (CtxWITV (xvalues, comparator))
    | None, Chooser choices ->
      Some (CtxChooser choices)
    | None, NoGrade ->
      Some CtxNoGrade
    | Some _, (QCM _ | Grader _ | WITV _ | Chooser _ | NoGrade) ->
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
      return (EvaluationError (UnboundQuestion qid))

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
  let questions = Section (TNil, questions) in
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

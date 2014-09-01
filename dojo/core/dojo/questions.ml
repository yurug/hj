(** The following type definitions mimick the ones found in [std.aka]. *)

open Lwt
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

let flatten_string = flatten "" ( ^ ) ( ^ )

let cons xs x = x :: xs

let flatten_list s = List.rev (flatten [] cons cons s)

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
| Grader of string * string list * string
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
    | VData (DName "Grader", [expected_file; import_files; command]) ->
      let expected_file = flatten_string (template string expected_file) in
      let import_files = flatten_list (template string import_files) in
      let command = flatten_string (template string command) in
      Grader (expected_file, import_files, command)
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

  let vcat f = flatten "" ( ^ ) (fun s c -> s ^ "\n" ^ f c)

  let text t = t.value

  let statement = function
    | Text s -> flatten "" ( ^ ) (fun s t -> s ^ text t) s

  let context = function
    | QCM (choices, _) ->
      String.concat "\n" (List.(
        mapi (fun i -> (sprintf "%d. %s") (i + 1)) (map statement choices)
      ))
    | Grader (expected_file, _, _) ->
      sprintf "%s?" expected_file

  let rec questions level = function
    | Question (id, title, s, c) ->
      Printf.sprintf "[%s] %s\n%s\n%s\n"
        (flatten_string id)
        (flatten_string title)
        (vcat statement s)
        (vcat context c)

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
  | EvaluationDone of grade
  | EvaluationWaits
  | EvaluationHandled of evaluation_job_identifier
deriving (Json)

type evaluations =
  (identifier * evaluation_state) list
deriving (Json)

let string_of_evaluation_state = function
  | EvaluationError _ -> "error"
  | EvaluationDone g -> string_of_grade g
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
  | CtxGrader of string * string list * string

let grade_qcm expected_choices cs =
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

let grade_regexp = Str.regexp "GRADE \\([0-9]+\\) \\([0-9]+\\)/\\([0-9]+\\)"

let grade_program files cmd update =
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
      update (EvaluationDone { scores; trace })
  ) in
  (* Run the command. *)
  let cmd = Str.(global_replace (regexp "%seed") (Seed.to_string secret) cmd) in
  Sandbox.(exec files cmd observer)
  >>= function
    | `OK (job, persistence_id) ->
      return (EvaluationHandled job)
    | `KO e ->
      (* FIXME: Provide a more detailed diagnostic. *)
      return (EvaluationError ErrorDuringGraderExecution)

let evaluate_using_context
    exo_real_path answer_real_path context answer update
=
  match context, answer with
    | CtxQCM expected_choices, Choices cs ->
      return (grade_qcm expected_choices cs)
    | CtxGrader (expected_file, imported_files, command), File filename ->
      let files =
        answer_real_path expected_file
        :: List.map exo_real_path imported_files
      in
      (* FIXME: Check that filename' = expected_file. *)
      grade_program files command update
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
    | Some (title, statement, context_makers) ->
      begin match make_context context_makers with
        | None -> return (EvaluationError InvalidContextDescription)
        | Some context ->
          evaluate_using_context
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

let update_evaluations exo_real_path answer_real_path evaluations questions qid answer update =
  lwt state =
    evaluate exo_real_path answer_real_path questions qid answer update
  in
  return (update_evaluation qid state evaluations)

let evaluation_state evaluations qid =
  try
    List.assoc qid evaluations
  with Not_found ->
    EvaluationError NoAnswer

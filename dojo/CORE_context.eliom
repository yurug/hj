(** -*- tuareg -*- *)

{shared{

(** Evaluation context. *)

open Lwt

open CORE_entity
open CORE_identifier
open CORE_error_messages
open COMMON_pervasives

type rule =
  | Answer of string
deriving (Json)

type context =
  | Empty
  | Compose of rule * context
deriving (Json)

type t = context deriving (Json)

type criteria = string deriving (Json)

type grade = int * int deriving (Json)

type score = (criteria * grade) list deriving (Json)

let empty = Empty

let push r c = Compose (r, c)

let answer fname = Answer fname

let get_answer_form c =
  let rec aux last = function
  | Empty -> last
  | Compose (Answer fname, qs) -> aux (Some (`Filename fname)) qs
  in
  aux None c

}}

type job =
  | ExecutableJob of CORE_machine.command_job
deriving (Json)

let create_job submission rule notify =
  return (ExecutableJob 0)

let cancel_job job =
  return ()

type submission = string deriving (Json)

type evaluation_dynamic_state =
  | Unevaluated
  | Evaluated      of score
  | BeingEvaluated of job
deriving (Json)

type evaluation_state = {
  estate         : evaluation_dynamic_state;
  context        : context;
  esubmission    : submission_state option;
}

and submission_dynamic_state =
  | New
  | Handled

and submission_state = {
  submission    : submission;
  sstate        : submission_dynamic_state;
} deriving (Json)

let is_new_submission s = (s.sstate = New)

let new_submission sfname = { submission = sfname; sstate = New }

let evaluate esubmission current_evaluation context change =
  (** First, we determine the evaluation. *)
  let evaluation =
    match current_evaluation with
      | None -> { estate = Unevaluated; context; esubmission }
      | Some e -> e
  in

  (** Second, if this evaluation is already running, we cancel its job. *)
  begin match evaluation.estate with
    | Unevaluated | Evaluated _ -> return ()
    | BeingEvaluated j -> cancel_job j
  end

  (** Third, we generate a job corresponding to the evaluation
      rule applied to the submission. *)
  >> create_job esubmission context change

  (** Finally, we update the evaluation state and returns the evaluation. *)
  >>= fun job -> return { evaluation with estate = BeingEvaluated job }

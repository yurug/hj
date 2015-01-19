open Lwt
open Entity
open InMemory
open Identifier
open ExtPervasives
open ExtProcess

let path = Identifier.from_strings [ "aka" ]

let exercise_module = "hackojo.exercise <here@hackojo.org>"

let up () = VFS.(
  if not (exists path) then
    create exercise_module path
  else
    return (`OK ())
)

let exercise_identifier name =
  Identifier.(identifier_of_path (concat path (path_of_identifier name)))

type code_state =
  | Valid of Timestamp.t * Aka.t
  | NoCode
  | Error of Timestamp.t * string
deriving (Json)

module UserAnswers = Rb.Dict (struct
  type key = identifier deriving (Json)
  type image = Answers.identifier deriving (Json)
  let compare = Identifier.compare
end)

type internal_state = {
  contributors  : User.identifier list;
  code          : code_state;
  user_answers  : UserAnswers.t;
  collaborative : bool;
} deriving (Json)

type public_change =
  | UpdateCode
  | NewAnswers of User.identifier * Answers.identifier
  | ShareAnswers of User.identifier * User.identifier
  | ImportAnswer of User.identifier * Questions.identifier * User.identifier
  | SetCollaborativeMode of bool

let questions_of_final_env uid final_env =
  Aka.extract_questions final_env uid >>= function
    | Some questions -> return (Questions.ReifyFromAka.exercise questions)
    | None -> return Questions.empty_exercise (* FIXME *)

include Entity.Make (struct

  type data = internal_state deriving (Json)

  type change = public_change

  let kind = "exercise"

  let act _ _ = return ()

  let react state mdeps cs later =
    let update_code content =
      let return_error msg =
        return { content with code = Error (Timestamp.current (), msg) }
      in
      let return_valid a =
        return { content with code = Valid (Timestamp.current (), a) }
      in
      OnDisk.load_resource (identifier state) "source.aka" >>= function
        | `OK (source, _) ->
          (try_lwt
             lwt r = Aka.compile (identifier state) (Resource.content source) in
             let description = fst r in
             lwt final_env = Aka.execute (identifier state) description in

             Aka.perform_initialization final_env
             >> Aka.perform_notifications final_env


             >> UserAnswers.lwt_iter content.user_answers (fun uid a ->
               Answers.make a >>= function
                 | `OK a ->
                   lwt questions = questions_of_final_env uid final_env in
                   Answers.push_new_description a questions
                 | `KO _ -> return () (* FIXME *)
             ) >> return_valid description
           with
             (* FIXME: The following lacks consistency. *)
             | AkaError.LoadModule m ->
               return_error (Printf.sprintf "Failed to load module %s." (
                 string_of_identifier m)
               )
             | AkaError.Parse pos ->
               return_error (Position.string_of_pos pos ^ ": Syntax error.")
             | Errors.AkaError (msg) ->
               return_error (msg)
             | e ->
               (* FIXME: Should be removed. *)
               return_error (Printexc.to_string e)
          )

        | `KO _ ->
          (* FIXME: More informative message. *)
          return_error "internal error while loading source.aka"
    in
    let new_answers uid aid content =
      let user_answers = UserAnswers.add uid aid content.user_answers in
      return { content with user_answers }
    in

    let share_answers content sharer_uid uid =
      try_lwt
        let shared_answers = UserAnswers.lookup sharer_uid content.user_answers in
        Answers.add_contributor shared_answers uid
        >> return content
      with Not_found ->
        return content (* FIXME *)
    in

    let import_answer content dst_uid qid src_uid =
      try_lwt
        let dst_answers = UserAnswers.lookup dst_uid content.user_answers in
        let src_answers = UserAnswers.lookup src_uid content.user_answers in
        Answers.import_contributor_answer dst_answers dst_uid src_answers qid
        >> return content
      with Not_found ->
        return content (* FIXME *)
    in

    let apply_change content = function
      | UpdateCode ->
        update_code content
      | NewAnswers (uid, aid) ->
        new_answers uid aid content
      | ShareAnswers (sharer_uid, uid) ->
        if content.collaborative then share_answers content sharer_uid uid
        else return content (* FIXME *)
      | ImportAnswer (dst_uid, qid, src_uid) ->
        if content.collaborative then import_answer content dst_uid qid src_uid
        else return content (* FIXME *)
      | SetCollaborativeMode collaborative ->
        return { content with collaborative }
    in

    (* FIXME: Common pattern to be factorized out. *)
    let content0 = content state in
    lwt content = Lwt_list.fold_left_s apply_change content0 cs in
    if content == content0 then
      return NoUpdate
    else
      return (UpdateContent content)

  let current_version = "1.0"

  let converters = []

  let string_of_change = function
    | UpdateCode ->
      "update code"
    | NewAnswers (uid, aid) ->
      Printf.sprintf "new answers for %s" (string_of_identifier uid)
    | ShareAnswers (sharer_uid, uid) ->
      Printf.sprintf "%s contributes to %s"
        (string_of_identifier uid)
        (string_of_identifier sharer_uid)
    | ImportAnswer (dst_uid, qid, src_uid) ->
      Printf.sprintf "%s imports answer for %s from %s"
        (string_of_identifier dst_uid)
        qid
        (string_of_identifier src_uid)
    | SetCollaborativeMode mode ->
      Printf.sprintf "collaborative mode is set to %B" mode

end)

let create who name =
  let id = exercise_identifier name in
  make id >>= function
    | `OK _ -> return (`KO (`AlreadyExists (path_of_identifier id)))
    | `KO _ ->
      User.is_teacher who >>= function
        | true ->
          let data = {
            contributors  = [ User.identifier who ];
            code          = NoCode;
            user_answers  = UserAnswers.empty;
            collaborative = false;
          }
          in
          let init = (data, empty_dependencies, []) in
          make ~init id
        | false ->
          return (`KO `StudentsCannotCreateExercise)

(* FIXME: This is a common pattern here: change + block observation...
   FIXME: make it a combinator. *)
let update who exo =
  let now = Timestamp.current () in
  let uptodate t = Timestamp.compare now t <= 0 in
  change ~who:(User.identifier who) exo UpdateCode >>
    let rec wait () =
      observe ~who:(User.identifier who) exo
        (fun data -> return (content data).code) >>= function
          | Valid (t, _) when uptodate t -> return (`OK ())
          | Error (t, e) when uptodate t -> return (`KO (`InvalidCode e))
          | _ -> Lwt_unix.sleep 0.1 >> Lwt_unix.yield () >> wait ()
    in
    wait ()

let load_module ?(relative=true) id =
  let id = if relative then exercise_identifier id else id in
  make id >>= function
    | `OK exo -> begin observe exo
      (fun data -> return (content data).code) >>= function
        | Valid (_, a) -> return (`OK (exo, a))
        | _ -> return (`KO id)
    end
    | `KO _ -> return (`KO id)

let lookup_user_answers exo uid =
  lwt user_answers =
    observe exo (fun data ->
      return (content data).user_answers)
  in
  let answers_id = UserAnswers.lookup uid user_answers in
  Answers.make answers_id

let check_answers_consistency exo uid questions =
  try_lwt
    lookup_user_answers exo uid >>= function
    | `OK answers -> Answers.(
      lwt q = observe answers (fun data -> return (content data).description) in
      if q <> questions then (
        push_new_description answers questions
      ) else
        return ()
    )
    | `KO _ -> return () (* FIXME *)
  with _ -> return () (* FIXME *)

let questions id uid =
  load_module ~relative:false id >>= function
    | `OK (exo, cst) ->
      (* FIXME: The following environment could be cached since it
         FIXME: only depends on [cst]. *)
      lwt final_env = Aka.execute id cst in
      lwt questions = questions_of_final_env uid final_env in
      check_answers_consistency exo uid questions
      >> return (`OK questions)

    | `KO _ ->
      return (`KO (`InvalidModule id))

let user_answers exo uid =
  let id = identifier exo in
  let fresh () =
    questions id uid >>>= fun questions ->
    Answers.answers_for id uid questions >>>= fun answers ->
    change exo (NewAnswers (uid, Answers.identifier answers))
    >> return (`OK answers)
  in
  try_lwt
    lookup_user_answers exo uid >>= function
      | `OK a -> return (`OK a)
      | `KO _ ->
        (* Something nasty happened since the answers are referenced
           by the exercise but cannot be loaded. In that case, we
           try to regenerate the answers entity. *)
        fresh ()
  with Not_found -> fresh ()

let answer id uid qid a =
  make id >>>= fun exo ->
  user_answers exo uid >>>= fun answers ->
  Answers.push_new_answer answers qid uid a >> return (`OK ())

let refresh_evaluations id =
  make id >>>= fun exo ->
  (observe exo (fun data -> return (content data).user_answers)
   >>= fun answers -> UserAnswers.lwt_fold answers (fun uid a_id ret ->
     match ret with
       | `KO e -> return (`KO e)
       | `OK () ->
         questions id uid >>>= fun questions -> Answers.(
           make a_id >>>= fun a ->
           refresh_questions a questions >> return (`OK ()))
   ) (`OK ()))

let evaluation_state id uid qid =
  make id >>>= fun exo ->
  user_answers exo uid >>>= fun answers ->
  lwt s = Answers.evaluation_state answers qid in
  return (`OK s)

let initialization =
  AkaCST.set_loader (fun id -> load_module id >>= function
    | `OK (_, t) -> return (`OK t)
    | `KO e -> return (`KO e)
  )

let user_has_subscribed user_id exo_id =
  make exo_id >>= function
    | `KO _ -> return false (* FIXME *)
    | `OK exo ->
      lwt user_answers =
        observe exo (fun data ->
          return (content data).user_answers)
      in
      try_lwt
        ignore (UserAnswers.lookup user_id user_answers);
        return true
      with Not_found -> return false

let results_of_question exo_id qid =
  make exo_id >>= function
    | `KO _ -> return [] (* FIXME *)
    | `OK exo ->
      lwt user_answers =
        observe exo (fun data ->
          return (content data).user_answers)
      in
      let rs = ref [] in
      UserAnswers.lwt_iter user_answers (fun user answers_id ->
        Answers.make answers_id >>= function
          | `OK answers ->
            begin try_lwt
              lwt answer = Answers.answer_of_question answers qid in
              lwt evaluation_state = Answers.evaluation_state answers qid in
              lwt friends = Answers.contributors answers in
              list_push rs (user, friends, answer, evaluation_state, answers);
              return ()
              with _ -> return ()
            end
          | `KO _ ->
            return () (* FIXME *)
      )
      >> return !rs

let new_contributor exo_id user_id contributor_id =
  make exo_id >>>= fun exo ->
  change exo (ShareAnswers (user_id, contributor_id))
  >> return (`OK ())

let import_answer exo_id user_id question_id source_id =
  make exo_id >>>= fun exo ->
  change exo (ImportAnswer (user_id, question_id, source_id))
  >> return (`OK ())

let set_exercise_collaborative exo_id mode =
  make (exercise_identifier (identifier_of_string exo_id)) >>= function
    | `OK exo ->
      change exo (SetCollaborativeMode mode)
    | `KO e ->
      return () (* FIXME *)

let is_collaborative exo =
  observe exo (fun state -> return (content state).collaborative)

let _ =
  AkaInterpreter.set_exercise_collaborative := set_exercise_collaborative

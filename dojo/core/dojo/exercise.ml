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

type internal_state = {
  contributors : User.identifier list;
  code         : code_state;
  user_answers : Answers.identifier Dict.t
} deriving (Json)

type public_change =
  | UpdateCode
  | NewAnswers of User.identifier * Answers.identifier

include Entity.Make (struct

  type data = internal_state deriving (Json)

  type change = public_change

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
             return_valid (fst r)
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
      let user_answers = Dict.add uid aid content.user_answers in
      return { content with user_answers }
    in

    let apply_change content = function
      | UpdateCode ->
        update_code content
      | NewAnswers (uid, aid) ->
        new_answers uid aid content
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

end)

let create who name =
  let id = exercise_identifier name in
  make id >>= function
    | `OK _ -> return (`KO (`AlreadyExists (path_of_identifier id)))
    | `KO _ ->
      User.is_teacher who >>= function
        | true ->
          let data = {
            contributors = [ User.identifier who ];
            code = NoCode;
            user_answers = Dict.empty
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
          | _ -> Lwt_unix.yield () >> wait ()
    in
    wait ()

let load_module ?(relative=true) id =
  let id = if relative then exercise_identifier id else id in
  make id >>= function
    | `OK exo -> begin observe exo
      (fun data -> return (content data).code) >>= function
        | Valid (_, a) -> return (`OK a)
        | _ -> return (`KO id)
    end
    | `KO _ -> return (`KO id)

let questions id uid =
  load_module ~relative:false id >>= function
    | `OK cst ->
      (* FIXME: The following environment could be cached since it
         FIXME: only depends on [cst]. *)
      lwt final_env = Aka.execute id cst in
      lwt questions = Aka.extract_questions final_env uid in
      let questions = Questions.ReifyFromAka.exercise questions in
      return (`OK questions)

    | `KO _ ->
      return (`KO (`InvalidModule id))

let user_answers exo uid =
  let id = identifier exo in
  lwt user_answers =
    observe exo (fun data ->
      return (content data).user_answers)
  in
  try
    let answers_id = Dict.find uid user_answers in
    Answers.make answers_id
  with Not_found ->
    questions id uid >>>= fun questions ->
    Answers.answers_for id uid questions >>>= fun answers ->
    change exo (NewAnswers (uid, Answers.identifier answers))
    >> return (`OK answers)

let answer id uid qid a =
  make id >>>= fun exo ->
  user_answers exo uid >>>= fun answers ->
  Answers.push_new_answer answers qid uid a >> return (`OK ())

let refresh_evaluations id =
  make id >>>= fun exo ->
  (observe exo (fun data -> return (content data).user_answers)
   >>= fun answers -> Dict.fold_s (fun ret (uid, a_id) ->
     match ret with
       | `KO e -> return (`KO e)
       | `OK () ->
         questions id uid >>>= fun questions -> Answers.(
           make a_id >>>= fun a ->
           refresh_questions a questions >> return (`OK ()))
   ) (`OK ()) answers)

let evaluation_state id uid qid =
  make id >>>= fun exo ->
  user_answers exo uid >>>= fun answers ->
  lwt s = Answers.evaluation_state answers qid in
  return (`OK s)

let initialization =
  AkaCST.set_loader load_module

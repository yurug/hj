(** -*- tuareg -*- *)

(** Exercise entities. *)

open Lwt
open Eliom_parameter

open CORE_identifier
open CORE_onthedisk_entity
open CORE_inmemory_entity
open CORE_entity
open CORE_standard_identifiers
open CORE_error_messages
open CORE_questions
open COMMON_pervasives

{shared{

module C = CORE_description_CST

open CORE_identifier

type checkpoint = CORE_questions.checkpoint deriving (Json)

let string_of_checkpoint s = s

type assignment_kind = [ `Must | `Should | `Can | `Cannot ] deriving (Json)

type questions = CORE_questions.t deriving (Json)

type questions_value =
    (questions * CORE_questions.questions_result) option
deriving (Json)

type description = {
  assignment_rules : (assignment_kind * CORE_property.rule list) list;
  title            : string;
  questions        : questions;
  questions_value  : questions_value;
  cst              : C.exercise;
} deriving (Json)

type patch = CORE_errors.position * CORE_errors.position * string

}}

{client{
type data = description
}}

let raw_user_description_filename = "description.txt"

let eval_questions authors this c =
  lwt title, v, sources = CORE_questions.eval authors this c.questions in
  return ({ c with
    title;
    questions_value = Some (c.questions, v)
  }, sources)

exception Error of [ `UndefinedEntity of CORE_identifier.t
                   | `AlreadyExists   of CORE_identifier.path
                   | `SystemError     of string
                   | `NeedPatch       of patch]

type public_change =
  | NewAnswer of
      CORE_identifier.t list (** Authors *)
    * CORE_identifier.t      (** Answer  *)
  | EvalQuestions of CORE_identifier.t list
  | Update of questions
  | UpdateSource of C.exercise C.with_raw

let answer_of_dependency_kind = "answer_of"

include CORE_entity.Make (struct

  type data = description deriving (Json)

  type change = public_change

  let string_of_change = function
    | NewAnswer (authors, answer) ->
      Printf.sprintf "New answer %s from %s."
        (string_of_identifier answer)
        (String.concat ", " (List.map string_of_identifier authors))
    | EvalQuestions authors ->
      Printf.sprintf "Evaluate questions for %s."
        (String.concat ", " (List.map string_of_identifier authors))
    | Update _ -> "Update the questions' AST."
    | UpdateSource _ -> "Update the questions' CST."

  let update_description_source state raw =
    let source = CORE_source.make raw_user_description_filename raw in
    save_source state source

  let react state deps cs change_later =

    let make_change (sources, dependencies, content) = function
      | NewAnswer (authors, answer) ->
        (* FIXME: Is it correct ? *)
        return (
          sources,
          push dependencies (answer, (answer_of_dependency_kind, authors)),
          content
        )

      | EvalQuestions authors ->
        lwt content, xsources =
          eval_questions authors (CORE_inmemory_entity.identifier state) content
        in
        let source (sources, dependencies, content) (f, c) =
          let s = CORE_source.make f c in
          save_source (CORE_inmemory_entity.identifier state) s
          >>= function
            | `OK true ->
              return (s :: sources, dependencies, content)
            | `OK false ->
              return (sources, dependencies, content)
            | `KO e ->
              warn e;
              return (sources, dependencies, content) (* FIXME: handle error. *)
        in
        Lwt_list.fold_left_s source (sources, dependencies, content) xsources

      | Update (questions) ->
        let must_reset_value =
          match content.questions_value with
            | Some (qv, _) when qv <> questions ->
              true
            | _ ->
              false
        in
        if (not must_reset_value) && questions = content.questions
        then
          return (sources, dependencies, content)
        else
          let questions_value =
            if must_reset_value then None else content.questions_value
          in
          return (sources, dependencies, {
            content with questions; questions_value
          })

      | UpdateSource (raw, cst) -> begin
        let c = { content with cst } in
        update_description_source (CORE_inmemory_entity.identifier state) raw
        >>= function
          | `OK _ ->
            return (sources, dependencies, c)
          | `KO e ->
            warn e; return (sources, dependencies, c) (* FIXME: handle error. *)
      end
    in
    lwt s, d, c =
      Lwt_list.fold_left_s
        make_change
        ([], dependencies state, content state)
        cs
    in
    let push x x0 c cs = if x <> x0 then UpdateSequence (c, cs) else cs in
    let cs = NoUpdate in
    let cs = push s [] (UpdateSources s) cs in
    let cs = push c (content state) (UpdateContent c) cs in
    let cs = push d (dependencies state) (UpdateDependencies d) cs in
    return cs

end)

{shared{
let title d = d.title

let questions d = d.questions

let current_value d =
  match d.questions_value with
    | None -> None
    | Some (_, v) -> Some v
}}

let force_update id =
  make id >>= function
    | `OK e ->
      observe e (fun c -> return c) >>= fun _ -> return ()
    | `KO e ->
      warn e; return ()

let empty_description = "{\n  title = [].\n}"

let raw_user_description_source id =
  (** We force the update of [id] because some update of the source
      might be dangling. *)
  CORE_onthedisk_entity.load_source id raw_user_description_filename
  >>= function
    | `KO e ->
      warn e;
      return (CORE_source.make raw_user_description_filename empty_description)
    | `OK s ->
      return s

let raw_user_description_retrieve =
  server_function Json.t<CORE_identifier.t> (fun id ->
    raw_user_description_source id >>= fun s -> return (CORE_source.content s)
  )

{client{
let raw_user_description id : string Lwt.t =
  %raw_user_description_retrieve id
}}

let with_local_error e = raise (Error e)

let sub_id_from_user_string s =
  let ps = path_of_string s in
  identifier_of_path (concat exercises_path ps)

let initial_source_filenames = [
  raw_user_description_filename
]

let rec enumeration f = function
  | C.All -> All
  | C.Insert xs -> Insert (List.map f xs)
  | C.Remove xs -> Remove (List.map f xs)
  | C.Union xs -> Union (List.map (enumeration f) xs)

let rec questions_from_cst raw e cst =
  let wrap f t =
    let y = f t.C.node in
    { source = t; term = y }
  in

  let rec typ' = function
    | None -> None
    | Some ty -> Some (typ ty)

  and typ = function
    | C.TApp (C.TVariable v, tys) ->
      TApp (TVariable v, List.map typ tys)

  and term' (t : C.term') = wrap (function
    | C.Lit l -> Lit (literal l)
    | C.Template t -> template t
    | C.Variable p -> Variable (path p)
    | C.App (a, b) -> App (term' a, term' b)
    | C.Lam (x, ty, t) -> Lam (x, typ' ty, term' t)
    | C.Module mc -> Module (module_term mc)
  ) t

  and module_term mc =
    List.map module_component mc

  and module_component (l, ty, t) =
    let l =
      match l with
        | None -> CORE_identifier.fresh_label "_"
        | Some l -> l
    in
    (l, typ' ty, term' t)

  and path = function
    | C.PRoot i -> PRoot i
    | C.PThis -> PThis
    | C.PSub (p, l) -> PSub (path p, l)

  and template t =
    Module (List.map template_atom t)

  and unamed_field s =
    (CORE_identifier.fresh_label "_", None, s)

  and template_atom = function
    | C.Raw s -> unamed_field (term' (C.(locate_as s (Lit (LString s.node)))))
    | C.Code t -> unamed_field (term' t)
    | C.RawCode _ -> assert false (* by nested parsing fixpoint. *)

  and literal = function
    | C.LString s -> LString s
    | C.LInt x    -> LInt x
    | C.LFloat f  -> LFloat f
    | C.LUnit     -> LUnit

  and unit_ty = TApp (TVariable "unit", [])

  and make_let t1 ty t2 =
    let b = CORE_identifier.fresh_label "_" in
    let t2 = t2 b in
    App ({ term = Lam (b, ty, t2); source = t2.source }, term' t1)

  and program t = term' t

  in
  program cst

(** Compare an entity with the user description CST to decide if
    the user description is different from the entity. *)
and changed x questions cst =
  observe x (fun d -> return ((content d).questions <> questions))

(** Take an exercise [x] and a user description [cr] and produce a
    required change on [x] to be up-to-date with respect to [cr].

    In the meantime, [cr] may contain outdated information about the
    questions in which case we have to produce patches to apply to the
    user description [cr].

    Finally, if [cr] contains new question definitions, we generate
    requests for the user to confirm their creation.
*)
and change_from_user_description x cr =
  lwt source_changed =
    raw_user_description_source (identifier x) >>= fun s ->
    return (CORE_source.content s <> C.raw cr)
  in
  if source_changed then (
    change x (UpdateSource cr) >>= fun _ ->
      let cst = C.data cr in
      let questions = questions_from_cst (C.raw cr) x cst in
      lwt changed = changed x questions cst in
      if changed then
        change x (Update questions)
      else
        return ()
  ) else return ()

let eval e authors = change e (EvalQuestions authors)

let assignment_rule e k =
  observe e (fun c -> return (
    try
      CORE_property.conjs (List.assoc k (content c).assignment_rules)
    with Not_found -> CORE_property.True
  ))

let exercise_id username =
  identifier_of_path (
    concat exercises_path (CORE_identifier.make [label username])
  )

let _ =
  CORE_questions.set_import_exercise (fun id ->
    make id >>= function
      | `OK e -> observe e (fun d -> return (content d).questions)
      | _ -> assert false (* FIXME *)
  )

let eval_if_needed e authors =
  observe e (fun d -> return (content d).questions_value) >>= function
    | None -> eval e authors >>= fun _ -> return None
    | Some (_, v) -> return (Some v)

let context_of_checkpoint e c authors =
  eval_if_needed e authors >>= function
    | Some (`OK v) -> return (Some (CORE_questions.context_of_checkpoint v c))
    | _ -> return None

let all_checkpoints e authors =
  eval_if_needed e authors >>= function
    | Some (`OK v) -> return (CORE_questions.all_checkpoints v)
    | _ -> return []

let make_blank id =
  let assignment_rules = [] in
  let questions = CORE_questions.blank in
  let init = (
    { title = I18N.String.no_title;
      assignment_rules;
      questions;
      questions_value = None;
      cst = C.blank'
    },
    CORE_inmemory_entity.empty_dependencies,
    CORE_property.empty,
    initial_source_filenames
  ) in
  make ~init id

let create_service ok_page ko_page =
  Eliom_registration.Redirection.register_service
    ~path:["create_exercise"]
    ~get_params:Eliom_parameter.(suffix (list "id" (string "label")))
    (fun id () ->
      try_lwt
        let id = identifier_of_string_list id in
        make_blank id >>= function
          | `OK e ->
            return (ok_page e)
          | `KO e ->
            return (ko_page (string_of_error e))
      with InvalidLabel _ ->
       return (ko_page (string_of_error (`InvalidLabel (String.concat "/" id))))
    )

(** -*- tuareg -*- *)

(** Exercise entities. *)

open Lwt
open Eliom_parameter

open CORE_entity
open CORE_standard_identifiers
open CORE_error_messages
open CORE_questions
open COMMON_pervasives

{shared{

module C = CORE_description_CST

open CORE_identifier

type checkpoint = CORE_questions.checkpoint deriving (Json)

type assignment_kind = [ `Must | `Should | `Can | `Cannot ] deriving (Json)

type questions = CORE_questions.t deriving (Json)

type description = {
  assignment_rules : (assignment_kind * CORE_property.rule list) list;
  title            : string;
  questions        : questions;
  questions_value  : (questions * CORE_questions.questions_value) option;
  cst              : C.exercise;
} deriving (Json)

type patch = CORE_errors.position * CORE_errors.position * string

}}

{client{
type data = description
}}

let eval_questions this c =
  Ocsigen_messages.errlog "Evaluate questions.";
  lwt v = CORE_questions.eval this c.questions in
  return (Some {
    c with questions_value = Some (c.questions, v)
  })

exception Error of [ `UndefinedEntity of CORE_identifier.t
                   | `AlreadyExists   of CORE_identifier.path
                   | `SystemError     of string
                   | `NeedPatch       of patch]

include CORE_entity.Make (struct

  type data = description deriving (Json)

  let react this change_later deps new_data data =
    (** We reset the questions_value as a reaction to every
        change of one dependency or a change of the questions
        field. *)
    let data, must_reset_value =
      match new_data with
      | None ->
        (data, true) (* It means that one of the dependency changed. *)
      | Some ({ questions_value = Some (qv, _); questions } as data) ->
        (data, qv <> questions)
      | Some ({ questions_value = None } as data) ->
        (data, true)
    in
    if must_reset_value then (
      change_later (eval_questions this);
      return (Some { data with questions_value = None })
    )
    else return (Some data)

end)

{shared{
let title d = d.title

let questions d = d.questions

let current_value d =
  match d.questions_value with
    | None -> None
    | Some (_, v) -> Some v
}}

let (raw_user_description_filename,
     raw_user_description_source,
     raw_user_description_retrieve)
    = source "description.txt"

{client{
let raw_user_description id = %raw_user_description_retrieve id
}}

let with_local_error e = raise (Error e)

let sub_id_from_user_string s =
  let ps = path_of_string s in
  identifier_of_path (concat exercises_path ps)

let include_subs cst =
  let aux = function

    | C.Include (id, start, stop) ->
      let id = sub_id_from_user_string id.C.node in
      (make id >>= function
        | `OK e' ->
          lwt source = raw_user_description_source e' in
          let content = CORE_source.content source in
          let patch = (start, stop, content) in
          return ([patch])

        | `KO e -> with_local_error e)

    | _ -> return []
  in
  lwt lps = Lwt_list.map_s aux cst.C.questions in
  return (List.flatten lps)

let collect_on_subs cst f =
  let rec aux = function
    | C.Sub (id, def) ->
      let id = sub_id_from_user_string id.C.node in
      f id def

    | _ ->
      return []
  in
  lwt xs = Lwt_list.map_s aux cst.C.questions in
  return (List.flatten xs)

let new_inline_subs raw e cst =
  Ocsigen_messages.errlog ("Searching for new inline definitions");
  collect_on_subs cst (fun id def ->
    make id >>= function
      | `OK e' ->
        (** We enforce the fact that the entity [e] depends on [e']. *)
        push_dependency e "subs" [] (SomeEntity e')
        >> return []
      | `KO e ->
        return [(id, C.with_sub_raw raw def)]
  )

let outdated_subs e cst =
  Ocsigen_messages.errlog ("Searching for outdated definitions");
  collect_on_subs cst (fun id def ->
    !!> begin fun () ->
      make id >>>= fun e' ->
        (** There already is an exercise [e'] named [id]. *)
        lwt cst = observe e' (fun d -> return d.cst) in

        if not (C.equivalent_exercises cst def.C.node) then

            (** There is a conflict between the two definitions. *)

            observe e (fun data ->
              if List.for_all
                (fun x -> x >= timestamp e')
                (timestamp_of_sub data.questions id)
              then (
                return (`OK [])
              ) else
                (** The definition has a new statement. Update the
                    inline definition in the exercise. *)
                lwt source = raw_user_description_source e' in
                let content = CORE_source.content source in
                let patch = C.(def.start, def.stop, content) in (
                  return (`OK [patch])
                )
            )
         else return (`OK [])
   end with_local_error)

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

  let rec component = function
    | C.Import (tys, id, xs) ->
      return (
        Import (enumeration typ tys,
                CORE_identifier.identifier_of_string id.C.node,
                enumeration (fun x -> x) xs)
      )

    | C.Include _ ->
      (** Includes should have been processed at this point. *)
      assert false

    | C.Binding (l, ty, t) ->
      let l = match l with
        | None -> None
        | Some l -> Some (Local l)
      in
      return (Binding (l, typ' ty, term t))

    | C.Sub (id, def) ->
      let id = sub_id_from_user_string id.C.node in
      make id >>= function
        | `OK e' ->
          (lwt cst = observe e' (fun d -> return d.cst) in
           Ocsigen_messages.errlog "Consider pushing.";
           if cst <> def.C.node then
                (** At this point, the inline definition is necessarily new. *)
             !!> (fun () ->
               Ocsigen_messages.errlog "Pushing new inline def.";
               (* FIXME *)
               change_from_user_description e' (C.with_sub_raw raw def)
             ) with_local_error
           else return []
          ) >>= fun _ -> return (Sub (id, timestamp e'))
        | `KO e -> with_local_error e

  and typ' = function
    | None -> None
    | Some ty -> Some (typ ty)

  and typ = function
    | C.TApp (C.TVariable v, tys) ->
      TApp (TVariable v, List.map typ tys)

  and term (t : C.term') = wrap (function
    | C.Lit l -> Lit (literal l)
    | C.Variable v -> Variable (Local v) (* FIXME: Generalize to external identifiers. *)
    | C.App (a, b) -> App (term a, term b)
    | C.Lam (x, ty, t) -> Lam (x, typ' ty, term t)
    | C.Seq [] -> assert false
    | C.Seq [x] -> (term x).term
    | C.Seq (x :: xs) ->
      make_let x (Some unit_ty) (fun _ -> term (C.locate_as t (C.Seq xs)))
    | _ -> (* FIXME *) assert false
  ) t

  and literal = function
    | C.LString s -> LString s
    | C.LInt x    -> LInt x
    | C.LFloat f  -> LFloat f
    | C.LUnit     -> LUnit

  and unit_ty = TApp (TVariable "unit", [])

  and make_let t1 ty t2 =
    let b = CORE_identifier.fresh_label "_" in
    let t2 = t2 b in
    App ({ term = Lam (b, ty, t2); source = t2.source }, term t1)

  in
  Lwt_list.map_s component cst.C.questions

(** Compare an entity with the user description CST to decide if
    the user description is different from the entity. *)
and changed x questions cst =
  lwt o1 = observe x (fun d -> return (d.questions <> questions)) in
  lwt o2 = observe x (fun d -> return (d.title <> cst.C.title.C.node)) in
  return (o1 || o2)

(** Take an exercise [x] and a user description [cr] and produce a
    required change on [x] to be up-to-date with respect to [cr].

    In the meantime, [cr] may contain outdated information about the
    questions in which case we have to produce patches to apply to the
    user description [cr].

    Finally, if [cr] contains new question definitions, we generate
    requests for the user to confirm their creation.
*)
and change_from_user_description x cr =
  try_lwt
    let cst = C.data cr in
    include_subs cst >>= function
      | [] ->
        (new_inline_subs (C.raw cr) x cst >>= function
          | [] ->
            (outdated_subs x cst >>= function
              | [] ->
                Ocsigen_messages.errlog ("Pull done, let us push.");
                lwt source = raw_user_description_source x in
                if CORE_source.content source <> (C.raw cr) then (
                  lwt questions = questions_from_cst (C.raw cr) x cst in
                  lwt changed = changed x questions cst in
                  (if changed then
                      let data = {
                        title = cst.C.title.C.node;
                        assignment_rules = [];
                        questions;
                        questions_value = None;
                        cst
                      } in
                      CORE_source.set_content source (C.raw cr);
                      change x (fun data_now -> return (Some data))
                   else return ())
                  >> return (`OK [])
                ) else return (`OK [])
              | p :: _ ->
                Ocsigen_messages.errlog ("Patch needed");
                return (`KO (`NeedPatch p))
            )
          | new_inline_questions ->
            Ocsigen_messages.errlog ("New inline definitions");
            return (`OK new_inline_questions)
        )
      | p :: _ ->
        return (`KO (`NeedPatch p))
  with Error e ->
    return (`KO e)

let eval e = change e (eval_questions (identifier e))

let assignment_rule e k =
  observe e (fun c -> return (
    try
      CORE_property.conjs (List.assoc k c.assignment_rules)
    with Not_found -> CORE_property.True
  ))

let exercise_id username =
  identifier_of_path (
    concat exercises_path (CORE_identifier.make [label username])
  )

let rec eval_if_needed e =
  observe e (fun d -> return d.questions_value) >>= function
    | None -> eval e >> eval_if_needed e
    | Some (_, v) -> return v

let _ =
  CORE_questions.set_import_exercise (fun id ->
    make id >>= function
      | `OK e -> observe e (fun d -> return d.questions)
      | _ -> assert false (* FIXME *)
  )

let context_of_checkpoint e c =
  eval_if_needed e >>= function
    | `OK v -> return (CORE_questions.context_of_checkpoint v c)
    | `KO _ -> return CORE_context.empty

let all_checkpoints e =
  eval_if_needed e >>= function
    | `OK v -> return (CORE_questions.all_checkpoints v)
    | `KO _ -> return []

let make_blank id =
  let assignment_rules = [] in
  let questions = [] in
  let init = (
    { title = I18N.String.no_title;
      assignment_rules;
      questions;
      questions_value = None;
      cst = C.blank
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

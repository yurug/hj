(** -*- tuareg -*- *)

(** Exercise entities. *)

open Lwt
open Eliom_parameter

open CORE_entity
open CORE_standard_identifiers
open CORE_error_messages
open COMMON_pervasives

{shared{

module C = CORE_description_CST

open CORE_identifier

type composer = Par | Seq deriving (Json)

type questions =
  | Compose           of composer * questions list
  | Statement         of string * questions
  | Checkpoint        of CORE_identifier.t
  | Sub               of CORE_identifier.t * CORE_entity.timestamp
 deriving (Json)

type assignment_kind = [ `Must | `Should | `Can | `Cannot ] deriving (Json)

type description = {
  assignment_rules : (assignment_kind * CORE_property.rule list) list;
  title            : string;
  questions        : questions;
  cst              : C.exercise;
} deriving (Json)

}}

let timestamp_of_sub questions rkey =
  let rec aux = function
    | Compose (_, qs) -> List.flatten (List.map aux qs)
    | Statement (_, qs) -> aux qs
    | Sub (r, ts) when r = rkey -> [ ts ]
    | Sub _ -> []
    | Checkpoint _ -> []
  in
  aux questions

{client{
type data = description
}}

type patch = C.position * C.position * string

exception Error of [ `UndefinedEntity of CORE_identifier.t
                   | `AlreadyExists   of CORE_identifier.path
                   | `SystemError     of string
                   | `NeedPatch       of patch]

include CORE_entity.Make (struct

  type data = description deriving (Json)

  let react = passive

end)

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
  let rec aux = function
    | C.Compose (_, qs) ->
      lwt nqs = Lwt_list.map_s aux qs in
      return (List.flatten nqs)

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
  aux cst.C.questions

let collect_on_subs cst f =
  let rec aux = function
    | C.Compose (_, qs) ->
      lwt nqs = Lwt_list.map_s aux qs in
      return (List.flatten nqs)

    | C.Include _ ->
      return []

    | C.Statement (_, q) ->
       aux q

    | C.Checkpoint _ ->
      return []

    | C.Sub (id, def) ->
      let id = sub_id_from_user_string id.C.node in
      f id def
  in
  aux cst.C.questions

let new_inline_subs raw e cst =
  Ocsigen_messages.errlog ("Searching for new inline definitions");
  collect_on_subs cst (fun id def ->
    make id >>= function
      | `OK e' ->
        (** We enforce the fact that the entity [e] depends on [e']. *)
        push_dependency e "subs" [] (SomeEntity e');
        return []
      | `KO e ->
        match def with
          | None ->
            with_local_error e
          | Some def ->
            return [(id, C.with_sub_raw raw def)]
  )

let outdated_subs e cst =
  Ocsigen_messages.errlog ("Searching for outdated definitions");
  collect_on_subs cst (fun id def ->
    !!> begin fun () -> match def with
      | Some def ->

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

      | None -> return (`OK [])

   end with_local_error)

let initial_source_filenames = [
  raw_user_description_filename
]

let rec questions_from_cst raw e cst =
  let composer_from_cst = function
    | C.Par -> Par
    | C.Seq -> Seq
  in
  let rec aux = function
    | C.Compose (c, qs) ->
      lwt qs = Lwt_list.map_s aux qs in
      return (Compose (composer_from_cst c, qs))

    | C.Include _ ->
      (** Includes should have been processed at this point. *)
      assert false

    | C.Checkpoint id ->
      (* FIXME: Should probably be rooted at [e.id]. *)
      return (Checkpoint (CORE_identifier.identifier_of_string id.C.node))

    | C.Statement (s, qs) ->
      lwt qs = aux qs in
      return (Statement (s.C.node, qs))

    | C.Sub (id, def) ->
      let id = sub_id_from_user_string id.C.node in
      make id >>= function
        | `OK e' -> (match def with
            | None -> return []
            | Some def ->
              lwt cst = observe e' (fun d -> return d.cst) in
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
  in
  aux cst.C.questions

(** Take an exercise [x] and a user description [cr] and produce a
    change on [x] to be up-to-date with respect to [cr].

    In meantime, [cr] may contain outdated information about the
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
                  lwt changed =
                    observe x (fun d -> return (d.questions <> questions))
                  in
                  (if changed then
                      let data = {
                        title = cst.C.title.C.node;
                        assignment_rules = [];
                        questions;
                        cst
                      } in
                      CORE_source.set_content source (C.raw cr);
                      change x (fun data_now -> return data)
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

let make_blank id =
  let assignment_rules = [] in
  let questions = Compose (Seq, []) in
  let init = (
    { title = I18N.String.no_title;
      assignment_rules;
      questions;
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

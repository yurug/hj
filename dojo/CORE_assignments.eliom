(** -*- tuareg -*- *)

(** Assigments entities. *)

(** There is a global unique entity responsible for the
    assignments of exercises to students.

    When an exercise is created, a rule of assignment must be
    given. It is used to decide at a given date, what are the
    assignments of a given user with a given set of properties.

    This decision procedure is centralized because we do not want
    to determine the assignments in a time proportional to the
    number of exercises. We'd rather construct a global decision
    tree so that this decision is merely proportional to the
    number of assignment rules.
*)

open Lwt

open CORE_entity
open CORE_inmemory_entity
open CORE_identifier
open CORE_error_messages
open COMMON_pervasives

type rule = CORE_property.rule deriving (Json)

(* FIXME: compile a decision tree. *)
type rules = (rule * CORE_exercise.reference list) list
    deriving (Json)

type assignment_kind = [ `Must | `Should | `Can | `Cannot ] deriving (Json)

type description = {
  rules   : (assignment_kind * rules) list;
} deriving (Json)

include CORE_entity.Make (struct

  type data = description deriving (Json)

  let react = passive

end)

lwt assigner =
  let initialize () =
    make
      ~init:({ rules = [] }, empty_dependencies, CORE_property.empty)
      CORE_standard_identifiers.assigner
  in
  make CORE_standard_identifiers.assigner >>= function
    | `KO (`UndefinedEntity _) ->
      (** The assigner does not exist. *)
      (initialize () >>= function
        | `OK e -> return e
        | `KO e -> fatal_error e
      )

    | `KO e ->
      fatal_error e

    | `OK e ->
      return e

let rules k =
  observe assigner (fun e -> return (
    try List.assoc k e.rules with Not_found -> []
  ))

let push_assignment_rule k r e =
  change assigner (fun a ->
    let rec aux = function
      | (k', rs) :: ks when k = k' -> (k, (r, [e]) :: rs) :: ks
      | k :: ks -> k :: aux ks
      | [] -> [(k, [(r, [e])])]
    in
    return { rules = aux a.rules }
  )

let assignments k u =
  lwt rules = rules k in
  let exo_refs = List.flatten (snd (List.split (List.filter (
    fun (r, _) ->
      let ps = CORE_user.properties u in
      CORE_property.evaluate ps r
  ) rules)))
  in
  (** If the system is in a coherent state, the dereferencing of
      the exercise references must be OK. *)
  (* FIXME: Check that and take the right decision if it is an
     invalid assumption in practice. *)
  Lwt_list.map_p (fun r -> CORE_exercise.deref r >>= function
    | `OK e -> return e
    | `KO _ -> assert false)
  exo_refs

let all_assignment_kinds = [ `Must; `Should; `Can; `Cannot ]

let register_exercise : CORE_exercise.t -> unit Lwt.t =
  fun e ->
    Lwt_list.iter_s (fun k ->
      lwt r = CORE_exercise.assignment_rule e k in
      push_assignment_rule k r (CORE_exercise.refer_to e)
    ) all_assignment_kinds

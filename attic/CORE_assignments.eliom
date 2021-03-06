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
type rules = (rule * CORE_identifier.t list) list
    deriving (Json)

type assignment_kind = [ `Must | `Should | `Can | `Cannot ] deriving (Json)

type description = {
  rules   : (assignment_kind * rules) list;
  extra_fields : (string * string) list;
} deriving (Json)

include CORE_entity.Make (CORE_entity.Passive (struct

  type data = description deriving (Json)

  let string_of_replacement _ = "Update assignment rules."

  let current_version = "1.0"
  let converters = []

end))

let assigner () =
  let initialize () =
    make
      ~init:({ rules = []; extra_fields = [] },
             empty_dependencies, CORE_property.empty, [])
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
  lwt a = assigner () in
  observe a (fun e -> return (
    try List.assoc k (content e).rules with Not_found -> []
  ))

let register_rule e new_rules =
  lwt a = assigner () in
  lwt content = observe a (fun s -> return (content s)) in
  let rules = content.rules in
  let cleanup ids =
    (** By the way, garbage collect identifiers. *)
    List.filter CORE_entity.exists ids
  in
  let refresh rules = List.(
    filter (fun (_, ids) -> ids <> []) (
      map (fun (r, ids) -> (r, cleanup (filter (fun e' -> e <> e') ids))) rules
    )
  )
  in
  let rules = List.map (fun (k, rules) -> (k, refresh rules)) rules in
  let register rules (k, r) =
    let rec aux = function
      | (k', rs) :: ks when k = k' -> (k, (r, [e]) :: rs) :: ks
      | k :: ks -> k :: aux ks
      | [] -> [(k, [(r, [e])])]
    in
    aux rules
  in
  change a (UpdateContent {
    content with rules = List.fold_left register rules new_rules
  })

let assignments k ps = List.(CORE_property.(
  lwt rules = rules k in
  let assignments = filter (fun (r, _) -> evaluate ps r) rules in
  let assignments = flatten (snd (split assignments)) in
  let assignments = filter CORE_entity.exists assignments in
  return assignments
))

let all_assignment_kinds = [ `Must; `Should; `Can; `Cannot ]

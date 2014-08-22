open InferenceTypes
open MultiEquation
open Name

(*<corrige>*)
type irule = variable list * crterm * (tname * variable) list

type krules = (variable * irule) list

let rules : (tname, krules) Hashtbl.t = Hashtbl.create 13

let rules_of k =
  try
    Hashtbl.find rules k
  with Not_found ->
    Hashtbl.add rules k [];
    []

exception Unsat

let rule_of_head_symbol g rs =
  List.find (fun (g', _) -> UnionFind.equivalent g g') rs

let rule_of k (g : variable) =
  let rs = rules_of k in
  let (_, (xs, instance, subgoals)) =
    try
      rule_of_head_symbol g rs
    with Not_found -> raise Unsat
  in
  let fresh_vars = List.map (fun _ -> variable Flexible ()) xs in
  let fresh_assoc = List.combine xs fresh_vars in
  let freshen t = change_arterm_vars fresh_assoc t in
  let freshen_sb (k, v) = (k, List.assoc v fresh_assoc) in
  (fresh_vars, freshen instance, List.map freshen_sb subgoals)

exception OverlappingInstances of tname * variable

(** [forall bs, k (g B) is equivalent to k1 b1 /\ .. kN bN]
    (where the [bi]s are members of the vector B) *)
let equivalent bs k g kbs =
  let rs = rules_of k in
  let ir = (bs, app (TVariable g) (List.map (fun v -> TVariable v) bs), kbs) in
  try
    ignore (rule_of_head_symbol g rs);
    raise (OverlappingInstances (k, g))
  with Not_found ->
    Hashtbl.replace rules k ((g, ir) :: rs)

let rec head_symbol' = function
  | App (a, _) -> head_symbol a
  | Var v -> v
  | _ -> assert false (* Because the input type is a datatype. *)

and head_symbol v =
    match variable_structure v with
      | None -> v
      | Some t -> head_symbol' t

let decompose_instance pos pool k t =
  let (xs, instance, subgoals) = rule_of k (head_symbol t) in
  try
    List.iter (introduce pool) xs;
    Unifier.unify pos (register pool) (chop pool instance) t;
    subgoals
  with _ ->
    assert false
    (** Because if the head symbol matches, the subterms must
        be unifiable as they are variables. *)

(** Canonicalization uses known equivalences to decompose the
    predicates of [r] into simpler ones. The resulting constraint is
    equivalent to [r] thanks to the restriction to non overlapping
    instances. The final constraint is either equivalent to CFalse if
    a predicate application is known to be unsatisfiable or a sequence
    of application of a class name to a type variable. *)
let canonicalize pos pool r =
  let rec canon (k, v) =
    match variable_structure v with
      | None ->
        let desc = UnionFind.find v in
        if desc.kind = Constant then
          decompose_instance pos pool k v
        else
          [(k, v)]
      | Some t -> decompose_instance pos pool k v
  in
  List.(flatten (map canon r))

type class_ancestors = tname list

let inheritance : (tname, class_ancestors) Hashtbl.t = Hashtbl.create 13

exception MultipleClassDefinitions of tname

exception UnboundClass of tname

(** [forall b, k b entails k1 b /\ .. kN b]. *)
let add_implication k ks =
  if Hashtbl.mem inheritance k then
    raise (MultipleClassDefinitions k)
  else
    Hashtbl.add inheritance k ks

let contains k' =
  let rec aux k =
    try
      k = k' || (
        let ancestors = Hashtbl.find inheritance k in
        List.(mem k' ancestors || exists aux ancestors)
      )
    with Not_found -> raise (UnboundClass k)
  in
  aux

let entails g r =
  List.(for_all (fun (k, _) -> exists (fun (k', _) -> contains k k') g) r)

(*</corrige>*)

(*<sujet>

(** [Unsat] is raised if a canonical constraint C ≡ false. *)
exception Unsat

(** [OverlappingInstances] is raised if two rules of kind (E) overlap. *)
exception OverlappingInstances of tname * variable

(** [MultipleClassDefinitions k] is raised if two rules of kind (I)
    share the same goal. *)
exception MultipleClassDefinitions of tname

(** [UnboundClass k] is raised if the type class [k] occurs in a
    constraint while it is undefined. *)
exception UnboundClass of tname

(** Student! This is your job! You must implement the following functions: *)

(** [equivalent [b1;..;bN] k t [(k_1,t_1);...;(k_N,t_N)]] registers
    a rule of the form (E). *)
let equivalent _ _ _ _ = ()

(** [canonicalize pos pool c] where [c = [(k_1,t_1);...;(k_N,t_N)]]
    decomposes [c] into an equivalent constraint [c' =
    [(k'_1,v_1);...;(k'_M,v_M)]], introducing the variables
    [v_1;...;v_M] in [pool]. It raises [Unsat] if the given constraint
    is equivalent to [false]. *)
let canonicalize _ _ k = k

(** [add_implication k [k_1;...;k_N]] registers a rule of the form
    (E'). *)
let add_implication _ _ = ()

(** [entails C1 C2] returns true is the canonical constraint [C1] implies
    the canonical constraint [C2]. *)
let entails _ _ = true

(** [contains k1 k2] *)
let contains _ _ = true

</sujet>*)

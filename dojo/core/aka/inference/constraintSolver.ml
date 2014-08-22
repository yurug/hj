(**************************************************************************)
(*  Adaptated from:                                                       *)
(*  Mini, a type inference engine based on constraint solving.            *)
(*  Copyright (C) 2006. François Pottier, Yann Régis-Gianas               *)
(*  and Didier Rémy.                                                      *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation; version 2 of the License.               *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful, but   *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  General Public License for more details.                              *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program; if not, write to the Free Software           *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *)
(*  02110-1301 USA                                                        *)
(*                                                                        *)
(**************************************************************************)

(** This module is a solver for typing constraints. *)

open Misc
open Name
open Positions
open TypeAlgebra
open InferenceTypes
open MultiEquation
open Positions
open Misc
open Constraint
open Unifier
open MultiEquation
open InferenceExceptions

exception Inconsistency

type tconstraint = Constraint.tconstraint

type canonical_constraint =  (tname * variable) list

let rtrue = []
let rconj c = List.flatten c
let rpredicate k c = [(k, c)]

type environment =
  | EEmpty
  | EEnvFrame of environment * string * canonical_constraint * variable

let environment_as_list e =
  let rec conv acu = function
    | EEmpty ->
        acu

    | EEnvFrame (env, name, c, v) ->
        conv ((name, (c, v))::acu) env
  in
    conv [] e

(** [lookup name env] looks for a definition of [name] within
    the environment [env]. *)
let rec lookup pos name = function
  | EEnvFrame (env, name', c, scheme) ->
      if name = name' then
        (c, scheme)
      else
        lookup pos name env

  | EEmpty ->
      raise (UnboundIdentifier (pos, Name name))

type occurrence = string * position

type answer = {
  bindings: (string * (variable list * canonical_constraint * variable)) list;
  instantiations: (occurrence * variable) list;
}

let empty_answer = {
  bindings = [];
  instantiations = []
}

let new_binding a n t =
  { a with bindings = (n, t) :: a.bindings }

let new_instantiation a n t =
  { a with instantiations = (n, t) :: a.instantiations }

let lookup_binding a n = List.assoc n a.bindings

let lookup_instantiation a i = List.assoc i a.instantiations

(* [generalize] *)

let generalize old_pool young_pool =

  (* We examine the variables in the young pool and sort them by rank
     using a simple bucket sort mechanism. (Recall that every variable
     in the young pool must have rank less than or equal to the pool's
     number.)  These variables are also marked as ``young'', so as to
     be identifiable in constant time. *)

  let young_number =
    number young_pool in

  let sorted =
    Array.create (young_number + 1) [] in

  let young =
    Mark.fresh() in

  List.iter (fun v ->
    let desc = UnionFind.find v in
    desc.mark <- young;
    let rank = desc.rank in
    try
      sorted.(rank) <- v :: sorted.(rank)
    with Invalid_argument _ ->
      (* The invariant is broken. *)
      failwith (Printf.sprintf "Out of bound when generalizing %s/%s"
                   (string_of_int rank)
                   (string_of_int (Array.length sorted)))
  ) (inhabitants young_pool);

  (* Next, we update the ranks of the young variables. One goal is to ensure
     that if [v1] is dominated by [v2], then the rank of [v1] is less than or
     equal to the rank of [v2], or, in other words, that ranks are
     nonincreasing along any path down the structure of terms.  The second
     goal is to ensure that the rank of every young variable is exactly the
     maximum of the ranks of the variables that it dominates, if there are
     any.

     The process consists of several depth-first traversals of the forest
     whose entry points are the young variables. Traversals stop at old
     variables. Roughly speaking, the first goal is achieved on the way
     down, while the second goal is achieved on the way back up.

     During each traversal, every visited variable is marked as such, so as
     to avoid being visited again. To ensure that visiting every variable
     once is enough, traversals whose starting point have lower ranks must
     be performed first. In the absence of cycles, this enforces the
     following invariant: when performing a traversal whose starting point
     has rank [k], every variable marked as visited has rank [k] or less
     already. (In the presence of cycles, this algorithm is incomplete and
     may compute ranks that are slightly higher than necessary.) Conversely,
     every non-visited variable must have rank greater than or equal to
     [k]. This explains why [k] does not need to be updated while going
     down. *)

  let visited =
    Mark.fresh() in

  for k = 0 to young_number do
    let rec traverse v =
      let desc = UnionFind.find v in

      (* If the variable is young and was not visited before, we immediately
         mark it as visited (which is important, since terms may be cyclic).
         If the variable has no structure, we set its rank to [k]. If it has
         some structure, we first traverse its sons, then set its rank to the
         maximum of their ranks. *)

      if Mark.same desc.mark young then begin
        desc.mark <- visited;
        desc.rank <- match desc.structure with
        | Some term ->
            fold (fun son accu ->
                      max (traverse son) accu
                 ) term IntRank.outermost
        | _ ->
            k
      end

      (* If the variable isn't marked ``young'' or ``visited'', then it must
         be old. Then, we update its rank, but do not pursue the computation
         any further. *)

      else if not (Mark.same desc.mark visited) then begin
        desc.mark <- visited;
        if k < desc.rank then
          desc.rank <- k
      end;

      (* If the variable was visited before, we do nothing. *)

      (* In either case, we return the variable's current (possibly updated)
         rank to the caller, so as to allow the maximum computation above. *)

      desc.rank

    in
      try
        Misc.iter traverse sorted.(k)
      with Invalid_argument _ ->
        (* The invariant is broken. *)
        failwith "Out of bound in traverse"

  done;

  (* The rank of every young variable has now been determined as precisely
     as possible.

     Every young variable that has become an alias for some other (old or
     young) variable is now dropped. We need only keep one representative
     of each equivalence class.

     Every young variable whose rank has become strictly less than the
     current pool's number may be safely turned into an old variable. We do
     so by moving it into the previous pool. In fact, it would be safe to
     move it directly to the pool that corresponds to its rank. However, in
     the current implementation, we do not have all pools at hand, but only
     the previous pool.

     Every young variable whose rank has remained equal to the current
     pool's number becomes universally quantified in the type scheme that is
     being created. We set its rank to [none]. *)

  for k = 0 to young_number - 1 do
    try
      List.iter (fun v ->
        if not (UnionFind.redundant v) then
          register old_pool v
      ) sorted.(k)
    with Invalid_argument _ ->
      (* The invariant is broken. *)
      failwith "Out of bound in young refresh."
  done;

  List.iter (fun v ->
    if not (UnionFind.redundant v) then
      let desc = UnionFind.find v in
      if desc.rank < young_number then
        register old_pool v
      else (
        desc.rank <- IntRank.none;
        if desc.kind = Flexible then desc.kind <- Rigid
      )
  ) sorted.(young_number)


(** [distinct_variables vl] checks that the variables in the list [vl]
    belong to distinct equivalence classes and that their structure is
    [None]. In other words, they do represent distinct (independent)
    variables (as opposed to nonvariable terms). *)
exception DuplicatedMark of Mark.t
let distinct_variables pos vl =
  let m = Mark.fresh() in
  try
    List.iter (fun v ->
      let desc = UnionFind.find v in
      match desc.structure with
        | Some _ ->
          raise (CannotGeneralize (pos, v))
        | _ ->
          if Mark.same desc.mark m then
            raise (DuplicatedMark m);
          desc.mark <- m
    ) vl
  with DuplicatedMark m ->
    let vl' =
      List.filter
        (fun v -> Mark.same (UnionFind.find v).mark m)
        vl
    in
    raise (NonDistinctVariables (pos, vl'))

(** [generic_variables vl] checks that every variable in the list [vl]
    has rank [none]. *)
let generic_variable v =
  let desc = UnionFind.find v in
  IntRank.compare desc.rank IntRank.none = 0

let generic_variables pos vl =
  List.iter (fun v ->
    if not (generic_variable v) then
      raise (CannotGeneralize (pos, v))
  ) vl

(* [solve] *)

let solve env pool c =
  let answer = ref empty_answer in

  (** [given_c] corresponds to the class predicates given
      by the programmer as an annotation. *)
  let rec solve env pool given_c c =
    let pos = cposition c in
    try
      solve_constraint env pool given_c c
    with Inconsistency -> raise (TypingError pos)

  and solve_constraint env pool given_c c =
    match c with

      | CTrue p ->
        rtrue

      | CDump p ->
        rtrue

      | CPredicate (pos, k, ty) ->
        (*<corrige>*)
        simplify pos pool given_c (rpredicate k (chop pool ty))
        (*</corrige>*)
        (*<sujet>
        (* Student! This is your job! *)
        rtrue
        </sujet>*)

      | CEquation (pos, term1, term2) ->
        let t1, t2 = twice (chop pool) term1 term2 in
        unify_terms pos pool t1 t2;
        rtrue

      | CConjunction cl ->
        rconj (List.map (solve env pool given_c) cl)

      | CLet ([ Scheme (_, [], fqs, [], c, _) ], CTrue _) ->
        (* This encodes an existential constraint. In this restricted
           case, there is no need to stop and generalize. The code
           below is only an optimization of the general case. *)
        List.iter (introduce pool) fqs;
        solve env pool given_c c

      | CLet (schemes, c2) ->
        let rs, env' =
          List.fold_left (fun (rs, env') scheme ->
            let (r, env'') = solve_scheme env pool given_c scheme in
            (r :: rs, concat env' env'')
          ) ([], env) schemes
        in
        rconj (solve env' pool given_c c2 :: rs)

      | CInstance (pos, SName name, term) ->
        let (c, t) = lookup pos name env in
        let ctys = List.map (fun (k, ty) -> ty) c in
        begin match instance pool (t :: ctys) with
          | [] -> assert false
          | instance :: itys ->
                    let t' = chop pool term in
            answer := new_instantiation !answer (name, pos) t';
            unify_terms pos pool instance t';
            (*<corrige>*)
            let c = List.map2 (fun (k, _) ty -> (k, ty)) c itys in
            simplify pos pool given_c c
            (*</corrige>*)
            (*<sujet>
            rtrue
            </sujet>*)
        end

      | CDisjunction cs ->
        assert false

  and solve_scheme env pool given_c = function

    | Scheme (_, [], [], [], c1, header) ->

      (* There are no quantifiers. In this restricted case,
         there is no need to stop and generalize.
         This is only an optimization of the general case. *)

      let solved_c = solve env pool given_c c1 in
      let henv = StringMap.map (fun (t, _) -> chop pool t) header in
      (rtrue, ([], solved_c, henv))

    | Scheme (pos, rqs, fqs, given_c1, c1, header) ->

      (* The general case. *)

      (*<corrige>*)
      let given_c = given_c @ given_c1 in
      let pool' = new_pool pool in
      List.iter (introduce pool') rqs;
      List.iter (introduce pool') fqs;
      let header = StringMap.map (fun (t, _) -> chop pool' t) header in
      let solved_c1 = solve env pool' given_c c1 in
      let solved_c1 = simplify pos pool' given_c solved_c1 in
      distinct_variables pos rqs;
      generalize pool pool';
      generic_variables pos rqs;
      let solved_c, hoisted_c =
        List.partition (fun (_, v) -> generic_variable v) solved_c1
      in
      let generalized_variables =
        List.filter (fun v ->
          let desc = UnionFind.find v in
          IntRank.compare desc.rank IntRank.none = 0)
          (inhabitants pool')
      in
      (hoisted_c, (generalized_variables, solved_c, header))
      (*</corrige>*)
      (*<sujet>
      let pool' = new_pool pool in
      List.iter (introduce pool') rqs;
      List.iter (introduce pool') fqs;
      let header = StringMap.map (fun (t, _) -> chop pool' t) header in
      let solved_c1 = solve env pool' given_c c1 in
      distinct_variables pos rqs;
      generalize pool pool';
      generic_variables pos rqs;
      let generalized_variables =
        List.filter (fun v ->
          let desc = UnionFind.find v in
          IntRank.compare desc.rank IntRank.none = 0)
          (inhabitants pool')
      in
      (rtrue, (generalized_variables, solved_c1, header))
      </sujet>*)

  and concat env (vs, c, header) =
    StringMap.fold (fun name v env ->
      answer := new_binding !answer name (vs, c, v);
      EEnvFrame (env, name, c, v)
    ) header env

  and unify_terms pos pool t1 t2 =
    try
      unify pos (register pool) t1 t2
    with Unifier.CannotUnify (pos, v1, v2) ->
      raise (IncompatibleTypes (pos, v1, v2))

  (*<corrige>*)
  and simplify pos pool given_c r =
    (* [r] is the residual constraint that must be simplified.
       If [given_c] is not CTrue then we must check that [given_c]
       entails [r]. *)
    ConstraintSimplifier.(try
      let r = canonicalize pos pool r in
      match given_c with
        | [] -> r
        | g when entails given_c r -> g
        | g -> raise Inconsistency
    with
      | Unsat ->
        raise Inconsistency
    )
  (*</corrige>*)

  in (
    ignore (solve env pool [] c);
    !answer
  )

(** [init] produces a fresh initial state. It consists of an empty
    environment and a fresh, empty pool. *)
let init () =
  EEmpty, MultiEquation.init ()

(** The public version of [solve] starts out with an initial state. *)
let solve c =
  let env, pool = init () in
  solve env pool c

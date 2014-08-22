(**************************************************************************)
(* Adapted from:                                                          *)
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

(** We follow the convention that types and type schemes are represented
    using the same data structure. In the case of type schemes, universally
    quantified type variables are distinguished by the fact that their rank
    is [none]. The pretty-printer binds these variables locally, while other
    (non-quantified) type variables are considered part of a global namespace.
*)

open Positions
open Misc
open Name
open Types
open InferenceTypes
open MultiEquation

(** The things that we export are [variable]s, that is, entry points
    for types or type schemes. *)
type variable = MultiEquation.variable

(** [name_from_int i] turns the integer [i] into a type variable name. *)
let seen_username = Hashtbl.create 13
let used_name s = Hashtbl.mem seen_username s
let remember s = Hashtbl.add seen_username s ()
let rec name_from_int i =
  if i < 26 then
    String.make 1 (Char.chr (0x61 + i))
  else
    name_from_int (i / 26 - 1) ^ name_from_int (i mod 26)

(** [gi] is the last consumed number. *)
let gi =
   ref (-1)

(** [ghistory] is a mapping from variables to variable names. *)
let ghistory =
  ref []

(** [reset()] clears the global namespace, which is implemented
   by [gi] and [ghistory]. *)
let reset () =
  gi := -1;
  ghistory := []

let ty_app t1 t2 =
  match t1 with
    | TyApp (pos, t, xs) ->
      TyApp (pos, t, xs @ [t2])
    | TyVar (pos, t) ->
      TyApp (pos, t, [t2])

let string_of_label = function Name.LName s -> s

exception Cycle

(** [export is_type_scheme v] returns an external representation of
    the type or type scheme whose entry point is [v]. The parameter
    [is_type_scheme] tells whether [v] should be interpreted as a type
    or as a type scheme. Equirecursive types are not
    handled. Consecutive calls to [export] share the same variable
    naming conventions, unless [reset] is called in between. *)
let export is_type_scheme =
  (** Create marks to deal with cycles. *)
  let visiting = Mark.fresh () in

  (** Create a local namespace for this type scheme. *)
  let i = ref (-1)
  and history = ref [] in

  (** [bind v s h] assigns [s] to [v] in [h].
      - [s] must not be already used in [h].
      - we maintain the invariant that there is at most one binding
        for each variable [v]. *)
  let bind v s h =
    if not (List.exists (fun (v', _) -> UnionFind.equivalent v v') !h) then (
      assert (not (List.exists (fun (_, s') -> s = s') !h));
      let desc = UnionFind.find v in
      desc.name <- Some (TName s);
      h := (v, s) :: !h
    )
  in

  (** [assign_name v s h or_else] assigns name [s] to [v] if it is not
      already assigned to another variable in [h]. Otherwise, [or_else ()]
      is evaluated. *)
  let assign_name v s h or_else =
    if List.exists (fun (_, s') -> s = s') !h then
      or_else v
    else (
      remember s;
      bind v s h;
      s
    )
  in

  (** [name v] looks up or assigns a name to the variable [v]. When
      dealing with a type scheme, then the local or global namespace
      is used, depending on whether [v] is universally quantified or
      not. When dealing with a type, only the global namespace is
      used. *)

  let autoname v =
    let desc = UnionFind.find v in
    let prefix, c, h =
      if is_type_scheme && IntRank.compare desc.rank IntRank.none = 0
      then
        "'", i, history
      else
        "'_", gi, ghistory
    in
    try
      Misc.assocp (UnionFind.equivalent v) !h
    with Not_found -> (
      let rec assign_fresh v =
        incr c;
        let result = prefix ^ name_from_int !c in
        if used_name result then
          assign_fresh v
        else
          assign_name v result h assign_fresh
      in
      assign_fresh v
    )

  in
  let var_name v =
    let desc = UnionFind.find v in
    match desc.name with
      | Some (TName name) ->
        if desc.kind <> Constant then
          try
            Misc.assocp (UnionFind.equivalent v) !history
          with Not_found ->
            assign_name v name history autoname
        else
          name
      | _ -> autoname v
  in

  let var_or_sym v =
    let v = UnionFind.repr v in
    let (TName s) as name =
      match variable_name v with
        | Some (TName s) ->
          let desc = UnionFind.find v in
          assert (desc.structure = None);
          if is_type_scheme && IntRank.compare desc.rank IntRank.none = 0 then
            try
              TName (Misc.assocp (UnionFind.equivalent v) !history)
            with Not_found -> TName (assign_name v s history autoname)
          else
            TName s
        | None ->
          TName (var_name v)
    in
    if s.[0] = '\'' then
      TyVar (undefined_position, name)
    else
      TyApp (undefined_position, name, [])
  in

  let rec export_variable visited v =

    let is_visited v =
      Mark.same (UnionFind.find v).mark visiting
    in
    let desc = UnionFind.find v in

    if is_visited v then
      raise Cycle

    else match desc.structure with
      | None -> var_or_sym v
      | Some t ->
        desc.mark <- visiting;
        let t = export_term visited t in
        desc.mark <- Mark.none;
        t

  and export_term visited t =
    let rec export = function
      | App (v1, v2) ->
        let t1 = export_variable visited v1
        and t2 = export_variable visited v2 in
        ty_app t1 t2

      | Var v ->
        export_variable visited v

      | RowCons _
      | RowUniform _ ->
        (** Because we do not make use of rows in the source language. *)
        assert false

    in export t
  in
  let prefix visited tvs () =
    if is_type_scheme then
      List.fold_left
        (fun quantifiers v ->
          let desc = UnionFind.find v in
          match desc.structure with
            | Some _ ->
              quantifiers
            | _ ->
              try
                ignore (List.find (UnionFind.equivalent v) !visited);
                quantifiers
              with Not_found ->
                let name =
                  match var_or_sym v with
                    | TyVar (_, name) -> name
                    | _ -> assert false
                in
                if IntRank.compare desc.rank IntRank.none = 0 then (
                  visited := v :: !visited;
                  name :: quantifiers
                )
                else
                  quantifiers
        )
        []
        tvs
    else
      []
  in
  fun tvs v ->
    let visited = ref [] in
    let t = export_variable visited v in
    (prefix visited tvs (), t)

exception RecursiveType of Positions.position

let type_of_variable pos v =
  try
    snd (export false [] v)
  with Cycle -> raise (RecursiveType pos)

let export_class_predicate pos (k, ty) =
  match snd (export false [] ty) with
    | TyVar (_, v) -> [ClassPredicate (k, v)]
    | _ -> []

let canonicalize_class_predicates ts cps =
  let cps =
    List.filter (fun (ClassPredicate (_, t)) ->
      List.mem t ts
    ) cps
  in
  let cps = List.sort (fun (ClassPredicate (k1, _)) (ClassPredicate (k2, _)) ->
    Pervasives.compare k1 k2
  ) cps
  in
  let rec aux last = function
    | [] -> []
    | x :: xs ->
      match last, x with
        | Some (ClassPredicate (k, v1)), (ClassPredicate (k', v2)) ->
          if k = k' && v1 = v2 then
            aux last xs
          else
            (ClassPredicate (k', v2)) :: aux (Some x) xs
        | None, x ->
          x :: aux (Some x) xs
  in
  let remove_redundancy cs =
    let subsum (ClassPredicate (k1, v1)) (ClassPredicate (k2, v2)) =
      v1 = v2 && ConstraintSimplifier.contains k1 k2 && k1 <> k2
    in
    List.(filter (fun c -> not (exists (subsum c) cs)) cs)
  in
  remove_redundancy (aux None cps)

let type_scheme_of_variable =
  fun pos (vs, cps, v) ->
    try
      let export = export true in
      let (ts, ty) = export vs v in
      let cps = List.(flatten (map (export_class_predicate pos) cps)) in
      let cps = canonicalize_class_predicates ts cps in
      let fvs = InternalizeTypes.variables_of_typ ty in
      let ts = List.filter (fun (TName x) -> StringSet.mem x fvs) ts in
      TyScheme (ts, cps, ty)
    with Cycle -> raise (RecursiveType pos)

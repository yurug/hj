(**************************************************************************)
(*  Adaptated from:                                                       *)
(*  Mini, a type inference engine based on constraint solving.            *)
(*  Copyright (C) 2006. Fran�ois Pottier, Yann R�gis-Gianas               *)
(*  and Didier R�my.                                                      *)
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

open Positions
open Misc
open InferenceTypes
open Name

(** Multi-equations are reachable through variables. *)
type variable =
    descriptor UnionFind.point

(** A descriptor contains several pieces of information, the most
    important of which is the structure of the multi-equation, or,
    in other words, its unique non-variable member, if there is one.

    If the [structure] field is [None], then the multi-equation only
    has variable members. If it is [Some t], then the multi-equation
    contains a non-variable member, namely the term [t]. Note that
    [t] is a term whose head symbol belongs to the algebra and whose
    parameters are again variables. Thus, the unifier works with
    so-called ``small terms'' only.

    The [rank] field contains the rank currently attached to the
    multi-equation.

    The [mark] field is transient, and may be used by the unifier's
    client for any purpose. *)
and descriptor = {
    mutable structure : structure option;
    mutable rank      : IntRank.t;
    mutable mark      : Mark.t;
    mutable kind        : variable_kind;
    mutable name        : tname option;
    mutable pos         : position option;
    mutable var         : variable option
  }

and structure = variable InferenceTypes.term

(** A variable can be either flexible, rigid or a constant.
    In the two last cases, a name has been given by the user.
    - [Flexible] variable can be unified with other terms
    whereas [Rigid] and [Constant] variables cannot.
    - [Constant] type variable are used to denote user
    type constructor. Their names cannot be used for flexible
    or rigid variables. *)
and variable_kind = Rigid | Flexible | Constant

type crterm = variable InferenceTypes.arterm

type pool = {
            number        : int; (** The present pool's rank. *)
    mutable inhabitants : variable list
  }

let is_rigid v =
  let desc = UnionFind.find v in
    desc.kind = Rigid || desc.kind = Constant

let is_flexible v =
  let desc = UnionFind.find v in
    desc.kind = Flexible

(** [new_pool pool] returns a new empty pool, whose number is the successor
    of [pool]'s. *)
let new_pool pool = {
  number = pool.number + 1;
  inhabitants = []
}

(** [init] produces a fresh initial state. It consists of an empty
    environment and a fresh, empty pool. *)
let init () =
  {
    number = IntRank.outermost;
    inhabitants = []
  }

(** [register pool v] adds [v] to the pool [pool]. It is assumed that [v]'s
    rank is already set, so it is not modified. *)
let register pool v =
  pool.inhabitants <- v :: pool.inhabitants

(** [introduce pool v] adds [v] to the pool [pool]. [v]'s rank is set to the
    pool's number. It is assumed that it was previously uninitialized. *)
let introduce pool v =
  let desc = UnionFind.find v in
    desc.rank <- pool.number;
    register pool v

  (* TEMPORARY time to perform the occur check on the variables which we
     just ranked [none]. On peut 'eventuellement integrer l'occur check
     a la derniere passe de realisation? *)

  (* Every variable that was initially a member of [young_pool] may now be
     viewed as the entry point of a type scheme. The body of the type scheme
     is the term obtained by traversing the structure below the entry
     point. The type scheme's universally quantified variables have rank
     [none], while its free variables have nonnegative ranks.

     Note that, when considering several such variables, the type schemes
     that they represent may share some of their structure. No copying is
     involved. *)

(* [instance] *)

let instance pool vs =

  (* [get], [set], and [setp] implement a constant-time mapping from
     descriptors of rank [none] to variables. [mapped] allows determining
     whether a given descriptor belongs to the domain of the
     mapping. [associate] and [retrieve] respectively allow extending and
     looking up the mapping.

     In order to implement a constant-time mapping without wasting extra
     space, we re-use the descriptor's [rank] field, which is redundant at
     this point, since its value must be [none], and store a pointer in
     it. The field is to be viewed as containing a pointer if and only if
     the descriptor is marked with [m]. *)

  let m =
    Mark.fresh() in

  let setp desc =
    Mark.same desc.mark m

  and set desc v =
    desc.mark <- m;
    desc.var <- Some v

  and get desc =
    unSome desc.var
  in

  (* If [v] has rank [none], then [copy v] returns a copy of the variable
     [v], and copies its descendants recursively. The copy is registered in
     the current pool and given the current rank. If [v] has nonnegative
     rank, then [copy v] returns [v]. Only one copy per variable is created,
     even if a variable is found twice during the traversal. *)

  let rec copy v =

    let desc = UnionFind.find v in

    (* If a copy has been created already for this variable, return it. We
       must check this condition first, since we must not read [desc.rank]
       unless we know that the variable hasn't been copied yet. *)

    if setp desc then
      get desc

    (* Otherwise, check the variable's rank. If it is other than [none],
       then the variable must not be copied. *)

    else if IntRank.(compare desc.rank none <> 0 || desc.kind = Constant)
    then
      v

    (* Otherwise, the variable must be copied. We create a new variable,
       update the mapping, then update the new variable's descriptor. Note
       that the mapping must be updated before making a recursive call to
       [copy], so as to guarantee termination in the presence of cyclic
       terms. *)

    else
      let desc' =
          {
            structure = None;
            rank = pool.number;
            mark = Mark.none;
            kind = Flexible;
            name = (match desc.kind with Rigid -> None | _ -> desc.name);
            pos = None;
            var = None
          }
      in
      let v' = UnionFind.fresh desc' in
        register pool v';
        set desc v';
        let v' =
          match desc.structure with
            | None ->
                v'
            | Some term ->
                desc'.structure <- Some (map copy term);
                v'
        in
          v'

  (* If [v] was effectively copied by [copy], then [restore v] returns
     [v] to its original state (that is, restores its rank to [none])
     and restores its descendants recursively. If [v] was not copied,
     [restore v] has no effect. *)

  and restore v =
    let desc = UnionFind.find v in
    if setp desc then begin
      desc.mark <- Mark.none;
      desc.rank <- IntRank.none;
      match desc.structure with
        | None ->
            ()
        | Some term ->
            iter restore term
    end

  in

  (* We are now ready to take an instance of the type scheme whose
     entry point is [v]. It is simply a matter of copying [v] and
     its descendants, stopping at non-universally-quantified nodes.
     The copy process affects the type scheme, which must be restored
     afterwards. The whole process is linear in the size of the type
     scheme, that is, in the number of universally quantified nodes. *)
  let vs' = List.map copy vs in
  List.iter restore vs;
  vs'

(** [chop pool term] adds appropriate fresh variables and
    multi-equations to the environment and returns a variable
    which, according to these multi-equations, is equal to the term
    [term]. In other words, it turns a term of arbitrary depth into
    a variable, together with a number of multi-equations of depth
    at most one. *)
let rec chop pool = function

  | TVariable v ->
      v

  | TTerm term ->
      let v =
        UnionFind.fresh
          { (* TEMPORARY invoquer une fonction de c'reation dans Unifier? *)
            structure = Some (map (chop pool) term);
            rank = pool.number;
            mark = Mark.none;
            kind = Flexible;
            name = None;
            pos  = None;
            var = None
          } in
        register pool v;
        v

(** [chopi rank term] chops a term. Any freshly created variables
    receive rank [rank], but are not added to any pool. *)
let chopi rank term =
  let dummy : pool = {
    number = rank;
    inhabitants = []
  } in
  chop dummy term

(** [variable()] creates a new variable, whose rank is [none]. *)
let variable kind ?name ?structure ?pos () =
  UnionFind.fresh {
    structure = structure;
    rank = IntRank.none;
    mark = Mark.none;
    kind = kind;
    name = name;
    pos = pos;
    var = None
  }

let explode t =
  (* use of [chopi] is OK here, as this function is
     used only during pretty-printing *)
  let v = chopi IntRank.outermost t in
    match (UnionFind.find v).structure with
        None -> Var v
      | Some t -> t

let variable_name v =
  (UnionFind.find v).name

let is_structured v =
  (UnionFind.find v).structure <> None

let variable_structure v =
  (UnionFind.find v).structure

let is_redundant =
  UnionFind.redundant

let are_equivalent v1 v2 =
  UnionFind.equivalent v1 v2

let inhabitants p =
  p.inhabitants

let number p =
  p.number

(** [variable()] returns a new variable. *)
let variable kind ?name ?structure ?pos () =
  let structure =
    match structure with
      | Some t ->
          let v = chopi IntRank.none t in
            Some (Var v)
      | None -> None
  in
    variable kind ?name:name ?structure:structure ?pos:pos ()

(** [variable_list xs] allocates a fresh variable for every element in the
  list [xs], and returns both a list of these variables and an association
  list that maps elements to variables, viewed as types. *)
let variable_list kind xs =
  List.fold_right (fun x (vs, xts) ->
                     let v = variable kind () in
                       v :: vs, (x, TVariable v) :: xts
                  ) xs ([], [])

(** [variable_list_from_names f xs] allocates a fresh variable for every
  string in the list [xs], and returns both a list of these variables
  and an association list that maps elements to variables, viewed as types. *)
let variable_list_from_names kind xs =
  List.fold_right
    (fun x (vs, xts) ->
       let k, n = kind x in
       let v = variable k ?name:n () in
         v :: vs, (x, TVariable v) :: xts
    ) xs ([], [])

(** [variable_set xs] allocates a fresh variable for every element in the
    set [xs], and returns both a list of these variables and a map of
    elements to variables, viewed as types. *)
let variable_set kind xs =
  StringSet.fold
    (fun x (vs, xts) ->
       let k, n = kind (TName x) in
       let v = variable k ?name:n () in
         v :: vs, StringMap.add x ((TVariable v), undefined_position) xts
    ) xs ([], StringMap.empty)

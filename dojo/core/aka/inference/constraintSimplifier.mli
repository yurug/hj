(** This module implements reasoning about canonical constraints of the form:

    K_1 T_1 /\ ... K_N T_N

    using rules of the form:

    - forall b_1 ... b_N, k t ≡ k_1 t_1 /\ ... k_N t_N   (E)
    - forall b, k b => k_1 b /\ ... k_N b                (E')
*)

open Position
open Name
open MultiEquation

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

(** [equivalent [b1;..;bN] k t [(k_1,t_1);...;(k_N,t_N)]] registers
    a rule of the form (E). *)
val equivalent
  : variable list -> tname -> variable -> (tname * variable) list -> unit

(** [canonicalize pos pool c] where [c = [(k_1,t_1);...;(k_N,t_N)]]
    decomposes [c] into an equivalent constraint [c' =
    [(k'_1,v_1);...;(k'_M,v_M)]], introducing the variables
    [v_1;...;v_M] in [pool]. It raises [Unsat] if the given constraint
    is equivalent to [false]. *)
val canonicalize
  : position -> pool -> (tname * variable) list -> (tname * variable) list

(** [add_implication k [k_1;...;k_N]] registers a rule of the form
    (E'). *)
val add_implication
  : tname -> tname list -> unit

(** [entails C1 C2] returns true is the canonical constraint [C1] implies
    the canonical constraint [C2]. *)
val entails
  : (tname * variable) list -> (tname * variable) list -> bool

(** [contains k1 k2] *)
val contains
  : tname -> tname -> bool

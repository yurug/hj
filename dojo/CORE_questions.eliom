(** -*- tuareg -*- *)

(** This module defines the language of exercise description.

    Roughly speaking, a program written in that language evaluates into
    a sequence of bindings for values of the following kinds:

    - documents that describe the statement of the exercise ;

    - (inlined) sources ;

    - evaluation specifiers that describe the protocol of evaluation,
    i.e. how the answers are submitted, how the submissions are
    evaluated and how the diagnostic is transmitted to the users.

    A basic import mechanisms enable a subsequence of bindings of
    some exercise to be used in another one.

    The language is simply-typed and its primitives are normalizing,
    so the language is normalizing too. This is important because we
    do not want user programs to diverge.
*)

{shared{

open Lwt
open CORE_identifier
open COMMON_pervasives

type 'a enumerate =
  | All
  | Insert of 'a list
  | Remove of 'a list
  | Union of 'a enumerate list

and t = term'

and term =
  | Lit of literal
  | Variable of path
  | Lam of label * ty option * term'
  | App of term' * term'
  | Module of module_term
  | Import  of ty enumerate * path * CORE_identifier.label enumerate

and module_term = (label * ty option * term') list

and path =
  | PRoot of identifier
  | PThis
  | PSub of path * label

and term' = {
  source : CORE_description_CST.term';
  term   : term
}

and literal =
  | LUnit
  | LInt    of int
  | LFloat  of float
  | LString of string

and ty =
  | TApp of type_variable * ty list
  | TModule of module_type

and module_type = (label * ty) list

and type_variable = TVariable of string

deriving (Json)

type checkpoint = string deriving (Json)

let blank = { source = CORE_description_CST.blank'; term = Module [] }

let string_of_ty ty =
  let rec aux context = function
    | TApp (TVariable tcon, args) ->
      begin match tcon, args with
        | tcon, [] ->
          tcon
        | "->", [ity; oty] ->
          may_paren context tcon (
            aux `LeftOfArrow ity ^ " -> "
            ^ aux `RightOfArrow oty
          )
        | tcon, args ->
          tcon ^ paren (String.concat ", " (List.map (aux `AsArgument) args))
      end
    | TModule mt ->
      "{" ^ module_type mt ^ "}"
  and module_type mt =
    String.concat "; " (List.map binding_type mt)

  and binding_type (n, ty) =
    label_to_string n ^ " : " ^ aux `AsArgument ty

  and may_paren context tcon s =
    if (match context, tcon with
      | `LeftOfArrow, "->" -> true
      | _, _ -> false
    ) then paren s else s
  and paren s = "(" ^ s ^ ")"
  in
  aux `AsArgument ty

}}

{shared{

type questions_value = atomic_value list

and questions_result = [
  `OK of questions_value
| `KO of [ CORE_errors.all ]
]

and atomic_value =
  | CheckpointContext of checkpoint * CORE_context.t
  | Statement of string
  | Source of string * string
deriving (Json)

}}

type internal_errors = [
| `TypeError of CORE_description_CST.term' * ty * ty
| `NeedAnnotation of CORE_description_CST.term'
| `UnboundVariable of CORE_description_CST.term' * label
| `BadApplication of CORE_description_CST.term'
| `EvalError
] deriving (Json)

exception Error of internal_errors

let eraise e = raise (Error e)

(** Extract the context of some checkpoint in a value. *)
let context_of_checkpoint cv cp =
  let rec find = function
    | CheckpointContext (cp', c) :: _ when cp = cp' -> c
    | _ :: xs -> find xs
    | [] -> assert false (* FIXME *)
  in
  find cv

let all_checkpoints cv =
  let rec all = function
    | [] -> []
    | CheckpointContext (c, _) :: xs -> c :: all xs
    | _ :: xs -> all xs
  in
  all cv

(** Interpreter for the language. *)

(* FIXME: Consider making it tagless using GADTs. *)

let rec filter_map f l =
  let rec aux acc = function
    | [] ->
      List.rev acc
    | (k, v) :: xs ->
      match f k v with None -> aux acc xs | Some y -> aux (y :: acc) xs
  in
  aux [] l

let import_exercise : (identifier -> t Lwt.t) option ref = ref None

let set_import_exercise f = import_exercise := Some f

let do_import_exercise id = match !import_exercise with
  | None -> assert false
  | Some f -> f id

type filter = label -> bool

type origin =
  | This of identifier
  | That of identifier * filter

let filter_from_enumerate predicate =
  let rec make = function
    | All -> fun _ -> true
    | Insert xs -> fun id -> List.exists (predicate id) xs
    | Remove xs -> fun id -> not (List.exists (predicate id) xs)
    | Union es -> fun id -> List.for_all (fun a -> make a id) es
  in
  make

module TypeCheck = struct

  (** Invariant: bindings must be distinct. *)
  type environment = (label * ty) list

  (* FIXME: Check invariant *)
  let bind x ty e : environment  = (x, ty) :: e
  let lookup x (e : environment) = List.assoc x e

  (** Type algebra *)
  let constructor x = TVariable x
  let constant x  = TApp (constructor x, [])
  let int         = constant "int"
  let unit        = constant "unit"
  let float       = constant "float"
  let string      = constant "string"
  let statement   = constant "statement"
  let context     = constant "context"
  let checkpoint  = constant "checkpoint"
  let ( --> ) a b = TApp (constructor "->", [ a; b ])
  let ttemplate x = TApp (constructor "template", [ x ])

  exception ShouldBeFunctional of CORE_description_CST.term

  let destruct_arrow = function
    | TApp (TVariable "->", [a; b]) -> Some (a, b)
    | _ -> None

  let primitives = Hashtbl.create 13

  let primitive n ty = Hashtbl.add primitives (label n) ty

  let statement_constructors = List.iter
    (fun c -> primitive c (ttemplate statement --> statement))
    [
      "paragraph"; "code"; "statement"; "bold"; "italic"; "list"; "enumerate";
      "item"; "latex"; "ilatex"
    ]

  let statement_constructors_2 = List.iter
    (fun c -> primitive c (
      ttemplate statement --> statement
    ))
    [
      "section"; "subsection"; "question"
    ]

  let _ =
    primitive "checkpoint" ((unit --> unit) --> context);
    primitive "answer_in_file" (ttemplate string --> unit);
    primitive "answer_values_of" (ttemplate string --> unit);
    primitive "answer_choices_of" (ttemplate string --> unit);
    primitive "mark_using" (ttemplate string --> unit);
    primitive "mark_using_expected_values" (ttemplate string --> unit);
    primitive "mark_using_expected_choices" (ttemplate int --> unit);
    primitive "source" (ttemplate string --> (ttemplate string --> unit));
    primitive "timeout" (int --> unit)

  let lookup_primitive = Hashtbl.find primitives

  let rec compatible ty ty' =
    match ty, ty' with
      | TApp (TVariable "statement", []), TApp (TVariable "string", [])
      | TApp (TVariable "string", []), TApp (TVariable "statement", []) ->
        true
      | TApp (TVariable "template", [x]), TApp (TVariable "template", [y]) ->
        compatible x y
      | ty, ty' ->
        ty = ty'

  let rec check_term e t = function
    | None -> infer_term e t.source t.term
    | Some ty ->
      match t.term, destruct_arrow ty with
        | Lam (x, None, t), Some (ity, oty) ->
          check_term (bind x ity e) t (Some oty)
        | _, _ ->
          let ity = infer_term e t.source t.term in
          if not (compatible ty ity) then
            eraise (`TypeError (t.source, ty, ity));
          ity

  and infer_term e source = function
    | Lit l -> literal l
    | Variable x -> variable e source x
    | Lam (x, xty, t) -> lambda e source x t xty
    | App (a, b) -> app e source a b
    | Module mt -> module_term e mt

  and module_term e mt =
    let (_, mc) = list_foldmap module_component e mt in
    TModule mc

  and module_component e (l, ty, t) =
    let ty = check_term e t ty in
    (bind l ty e, (l, ty))

  and literal = function
    | LInt _ -> int
    | LString _ -> string
    | LFloat _ -> float
    | LUnit -> unit

  and variable e source = function
    | PSub (PThis, l) -> begin
      try lookup_primitive l with Not_found ->
        try lookup l e with Not_found ->
          eraise (`UnboundVariable (source, l))
    end
    | _ -> eraise `EvalError

  and app e source a b =
    match destruct_arrow (infer_term e a.source a.term) with
      | None -> eraise (`BadApplication source)
      | Some (ity, oty) -> ignore (check_term e b (Some ity)); oty

  and lambda e source x t = function
    | None -> eraise (`NeedAnnotation source)
    | Some ty -> ty --> (infer_term (bind x ty e) t.source t.term)

  let program this p =
    ignore (infer_term [] p.source p.term);
    return p

end

module Eval = struct

  open Eliom_content
  open Html5.D
  open Html5

  type state = CORE_context.t

  type value =
    | VContext of CORE_context.t
    | VInt of int
    | VFloat of float
    | VString of string
    | VStatement of string
    | VUnit
    | VClosure of environment * label * term
    | VPrimitive of (state -> value -> (state * value) Lwt.t)
    | VSource of string * string
    | VModule of environment

  and environment = (label * value) list

  let rec flatten_module join map t =
    join (List.map (map_module_component map) t)

  and map_module_component map (_, v) =
    map v

  let rec as_list = function
    | VModule t ->
      flatten_module List.flatten as_list t
    | v ->
      [v]

  let rec as_string = function
    | (VModule t) as v ->
      String.concat "" (List.map as_string (as_list v))
    | VString s ->
      s
    | VInt x ->
      string_of_int x
    | VFloat f ->
      string_of_float f
    | VUnit ->
      ""
    | _ -> ""

  let as_int = function
    | VInt s -> s
    | v -> eraise `EvalError

  let as_string_list v = List.map as_string (as_list v)

  let as_int_list v = List.map as_int (as_list v)

  let primitives = Hashtbl.create 13

  let primitive name f =
    Hashtbl.add primitives (CORE_identifier.label name) f

  let rec make_primitive () =

    let state_effect f =
      fun state x ->
        lwt y = f x in
        return (CORE_context.(push y state), VUnit)
    in
    let stateful name f =
      primitive name (state_effect f)
    in
    let qfunction f =
      fun s x ->
        lwt y = f x in
        return (s, y)
    in
    let functional name f =
      primitive name (qfunction f)
    in
    primitive "checkpoint" (fun state block ->
      (** Block with a local state. *)
      apply block state VUnit
      >>= fun (s, _) -> return (state, VContext s)
    );

    let enclose start stop s =
      Printf.sprintf "%s%s%s" start s stop
    in
    let html_of_string b c s =
      let c = match c with
        | None -> ""
        | Some c -> " class='" ^ c ^ "'"
      in
      enclose ("<" ^ b ^ c ^ ">") ("</" ^ b ^ ">") s
    in
    let html_constructor (s, b, c) =
      functional s (fun v -> return (
        VStatement (html_of_string b c (as_string v))
      ))
    in
    let _html_constructor2 (s, b1, c1, b2, c2) =
      functional s (fun v ->
        let s1 = as_string v in
        return (VPrimitive (fun s v ->
          let s2 = as_string v in
          return (s, VStatement (html_of_string b2 c2 (
            html_of_string b1 c1 s1 ^ s2
          ))
        ))
        ))
    in
    List.iter html_constructor [
      "statement", "div", None;
      "paragraph", "p", None;
      "code", "pre", None;
      "bold", "span", Some "bold";
      "italic", "span", Some "italic";
      "list", "ul", None;
      "enumerate", "ol", None;
      "item", "li", None;
      "section", "h1", None;
      "subsection", "h2", None;
      "question", "h3", None
    ];

    let marker (s, start, stop) =
      functional s (fun v ->
        let s = as_string v in
        return (VStatement (enclose start stop s)))
    in
    List.iter marker [
      "ilatex", "\\(", "\\)";
      "latex", "\\[", "\\]";
    ];

    stateful "answer_in_file" (fun v ->
      let s = as_string v in
      return (CORE_context.answer s)
    );

    stateful "answer_values_of" (fun v ->
      let s = as_string_list v in
      return (CORE_context.key_values s)
    );

    stateful "answer_choices_of" (fun v ->
      let s = as_string_list v in
      return (CORE_context.choices s)
    );

    stateful "mark_using" (fun v ->
      let s = as_string v in
      return (CORE_context.command s)
    );

    stateful "mark_using_expected_values" (fun v ->
      let s = as_string_list v in
      return (CORE_context.expected_values s)
    );

    stateful "mark_using_expected_choices" (fun v ->
      let c = as_int_list v in
      return (CORE_context.expected_choices c)
    );

    stateful "timeout" (function
      | VInt s -> return (CORE_context.timeout s)
      | _ -> eraise `EvalError
    );

    functional "source" (fun v ->
      let fname = as_string v in
      return (VPrimitive (qfunction (fun v ->
        let content = as_string v in
        return (VSource (fname, content)
        ))))
    );

  and variable s (e : environment) = function
    | PSub (PThis, l) ->
      begin try_lwt
              return (s, VPrimitive (Hashtbl.find primitives l))
        with Not_found ->
          try_lwt
            return (s, List.assoc l e)
          with Not_found -> eraise `EvalError
      end
    | _ ->
      eraise `EvalError (* FIXME *)

  and literal = function
    | LInt x -> VInt x
    | LString s -> VString s
    | LFloat f -> VFloat f
    | LUnit -> VUnit
  and closure s env x t =
    return (s, VClosure (env, x, t.term))

  and apply f s v : (state * value) Lwt.t =
    match f with
      | VClosure (e, x, t) ->
        term s ((x, v) :: e) t
      | VPrimitive p -> p s v
      | _ -> eraise `EvalError (* FIXME: Handle error. *)

  and term s e = function
    | Lit l ->
      return (s, literal l)
    | Variable x ->
      variable s e x
    | Lam (x, _, t) ->
      closure s e x t
    | App (a, b) ->
      lwt s, a = term' s e a in
      lwt s, b = term' s e b in
      apply a s b
    | Module mt ->
      module_term s e mt

  and term' s e t = term s e t.term

  and module_term s e mt =
    lwt (s, e, me) =
      Lwt_list.fold_left_s module_component (s, e, []) mt
    in
    return (s, VModule (List.rev me))

  and module_component (s, e, cs) (l, _, t) =
    lwt s, v = term' s e t in
    return (s, (l, v) :: e, (l, v) :: cs)

  let _ = make_primitive ()

  let values_of_module = function
    | VModule m ->
      m
    | _ ->
      []

  let program this p =
    lwt _, v = term' CORE_context.empty [] p in
    return (filter_map (fun x -> function
      | VStatement s ->
        Some (Statement s)
      | (VString _ | VModule _) as v ->
        Some (Statement ("<p>" ^ as_string v ^ "</p>"))
      | VContext c ->
        Some (CheckpointContext (label_to_string x, c))
      | VSource (s, c) ->
        Some (Source (s, c))
      | _ ->
        None
      ) (values_of_module v)
    )

end

(* FIXME: I18N and so on... *)
let convert_to_string_error
: internal_errors -> CORE_errors.all
= CORE_description_CST.(
  function
  | `TypeError (t, xty, ity) ->
    `TypeError (start_of t,
                Printf.sprintf "\nExpected type: %s\nActual type:%s"
                  (string_of_ty xty)
                  (string_of_ty ity)
    )
  | `NeedAnnotation t ->
    `NeedAnnotation (start_of t)
  | `UnboundVariable (t, n) ->
    `UnboundVariable (start_of t, label_to_string n)
  | `BadApplication t ->
    `BadApplication (start_of t)
  | `EvalError ->
    `EvalError
)

(** A well-typed exercise description evaluates into a value. *)
let eval this p : questions_result Lwt.t =
  try_lwt
(*    lwt tenv = TypeCheck.program this p in*)
    lwt v = Eval.program this p in
    return (`OK v)
  with Error e ->
    return (`KO (convert_to_string_error e))

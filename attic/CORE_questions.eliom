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
  | Statement of CORE_statement.t
  | Source of string * string
  | Title of string
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
  open CORE_statement

  type state = CORE_context.t

  type value =
    | VContext of CORE_context.t
    | VInt of int
    | VFloat of float
    | VString of string
    | VStatement of CORE_statement.t
    | VUnit
    | VClosure of environment * label * term
    | VPrimitive of (environment -> state -> value -> (state * value) Lwt.t)
    | VSource of string * string
    | VModule of environment
    | VPropertyRule of CORE_property.rule

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

  let rec as_statement = function
    | (VModule t) as v ->
      sequence (List.map as_statement (as_list v))
    | VStatement s ->
      s
    | v ->
      text (as_string v)

  let as_int = function
    | VInt s -> s
    | VString s -> begin try int_of_string s with _ -> eraise `EvalError end
    | v -> eraise `EvalError

  let as_string_list v = List.map as_string (as_list v)

  let as_int_list v = List.map as_int (as_list v)

  let primitives = Hashtbl.create 13

  let primitive name f =
    Hashtbl.add primitives (CORE_identifier.label name) f

  let rec make_primitive () =

    let state_effect f =
      fun env state x ->
        lwt y = f env x in
        return (CORE_context.(push y state), VUnit)
    in
    let stateful name f =
      primitive name (state_effect f)
    in
    let qfunction f =
      fun e s x ->
        lwt y = f e x in
        return (s, y)
    in
    let functional name f =
      primitive name (qfunction f)
    in
    primitive "checkpoint" (fun e state block ->
      (** Block with a local state. *)
      apply e block state VUnit
      >>= fun (s, _) -> return (state, VContext s)
    );

    let enclose start stop s =
      Printf.sprintf "%s%s%s" start s stop
    in
    let html_of_string b c s =
      let c = match c with
        | None -> ""
        | Some c -> " " ^ c
      in
      enclose ("<" ^ b ^ c ^ ">") ("</" ^ b ^ ">") s
    in
    let statement_constructor (s, make) =
      functional s (fun _ v -> return (
        VStatement (make [as_statement v])
      ))
    in
    List.iter statement_constructor [
      "statement", paragraph;
      "paragraph", paragraph;
      "verb", verb;
      "bold", bold;
      "italic", italic;
      "list", list;
      "enumerate", enumerate;
      "item", item;
      "section", section;
      "subsection", subsection;
      "question", question;
      "latex", latex;
      "ilatex", ilatex
    ];

    functional "code" (fun _ v ->
      let s = as_string v in
      return (VStatement (code [text s]))
    );

    functional "link" (fun _ v ->
      let caption = as_string v in
      return (VPrimitive (fun _ s v ->
        let url = as_string v in
        return (s, VStatement (link url caption))
      )));

    functional "source_link" (fun e v ->
      try
        match List.assoc (CORE_identifier.label "_this_path") e with
          | VString path ->
            let s = as_string v in
            lwt url = COMMON_file.send (Filename.concat path s) in
            return (VStatement (link url s))
          | _ ->
            assert false
      with _ -> assert false
    );

    functional "source_image" (fun e v ->
      try
        match List.assoc (CORE_identifier.label "_this_path") e with
          | VString path ->
            let s = as_string v in
            let filename = Filename.concat path s in
            lwt url = COMMON_file.send filename in
            return (VStatement (image url filename s))
          | _ ->
            assert false
      with _ -> assert false
    );

    stateful "choose_property" (fun _ v ->
      let s = as_string_list v in
      return (CORE_context.choose_property s)
    );

    stateful "master_focus" (fun _ v ->
      let s = as_string_list v in
      return (CORE_context.master_focus s)
    );

    functional "master_grade" (fun _ v ->
      let criteria = as_string v in
      return (VPrimitive (fun _ s v ->
        let over = as_int v in
        return (CORE_context.(push (master_grade criteria over) s), VUnit)
      )));

    stateful "answer_in_file" (fun _ v ->
      let s = as_string v in
      return (CORE_context.answer s)
    );

    stateful "answer_values_of" (fun _ v ->
      let s = as_string_list v in
      return (CORE_context.key_values s)
    );

    stateful "answer_choices_of" (fun _ v ->
      let s = as_string_list v in
      return (CORE_context.choices s)
    );

    stateful "mark_using" (fun _ v ->
      let s = as_string v in
      return (CORE_context.command s)
    );

    stateful "mark_using_expected_values" (fun _ v ->
      let s = as_string_list v in
      return (CORE_context.expected_values s)
    );

    stateful "mark_using_expected_choices" (fun _ v ->
      let c = as_int_list v in
      return (CORE_context.expected_choices c)
    );

    stateful "timeout" (fun _ -> function
      | VInt s -> return (CORE_context.timeout s)
      | _ -> eraise `EvalError
    );

    functional "source" (fun _ v ->
      let fname = as_string v in
      return (VPrimitive (qfunction (fun _ v ->
        let content = as_string v in
        return (VSource (fname, content)
        ))))
    );

    stateful "import_source" (fun _ v ->
      let fname = as_string v in
      return (CORE_context.source fname)
    );

    functional "is" (fun _ v ->
      let what = as_string v in
      return (VPropertyRule (CORE_property.(Is (atom what))))
    );

    functional "in_list" (fun _ v ->
      let what = as_string_list v in
      let rule = CORE_property.(
        disjs (List.map (fun v -> Is (atom v)) what)
      )
      in
      return (VPropertyRule rule)
    );

    functional "all" (fun _ v ->
      return (VPropertyRule (CORE_property.True))
    )

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

  and apply e f s v : (state * value) Lwt.t =
    match f with
      | VClosure (e, x, t) ->
        term s ((x, v) :: e) t
      | VPrimitive p -> p e s v
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
      apply e a s b
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
    let this_path =
      CORE_identifier.(CORE_standard_identifiers.(
        string_of_path (root true (path_of_identifier this))
      ))
    in
    let e = [CORE_identifier.label "_this_path", VString this_path] in
    lwt _, v = term' CORE_context.empty e p in
    let sources = ref [] in
    let title = ref "Sans titre" (* FIXME *) in
    let must = ref CORE_property.False in
    let can = ref CORE_property.False in
    let should = ref CORE_property.False in
    let v = (filter_map (fun x -> function
      | VStatement s ->
        Some (Statement s)

      | VPropertyRule r as v when x = CORE_identifier.label "must" ->
        must := r;
        None

      | VPropertyRule r as v when x = CORE_identifier.label "should" ->
        should := r;
        None

      | VPropertyRule r as v when x = CORE_identifier.label "can" ->
        can := r;
        None

      | (VString _ | VModule _) as v when x = CORE_identifier.label "title" ->
        title := as_string v;
        None

      | VContext c ->
        Some (CheckpointContext (label_to_string x, c))

      | VSource (s, c) ->
        sources := (s, c) :: !sources;
        None

      | _ ->
        None
      ) (values_of_module v)
    )
    in
    return (!title, v, !sources, !must, !can, !should)

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
let eval authors this p =
  let nil = CORE_property.False in
  try_lwt
(*    lwt tenv = TypeCheck.program this p in*)
    lwt title, v, sources, must, can, should = Eval.program this p in
    (** "must" or "should" imply "can". *)
    let can = CORE_property.disjs [can; must; should] in
    return (title, `OK v, sources, must, can, should)
  with
    | Error e ->
      return ("", `KO (convert_to_string_error e), [], nil, nil, nil)
    | _ ->
      Ocsigen_messages.errlog "Unexpected error during evaluation.";
      return ("", `KO `EvalError, [], nil, nil, nil)

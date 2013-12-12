(** -*- tuareg -*- *)

(** This module defines the language of exercise description.

    Roughly speaking, a program written in that language evaluates into
    a sequence of bindings for values of the following kinds:

    - documents that describe the statement of the exercise ;

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

type 'a enumerate =
  | All
  | Insert of 'a list
  | Remove of 'a list
  | Union of 'a enumerate list
deriving (Json)

type component =
  | Sub     of identifier * CORE_entity.timestamp
  | Binding of name option * ty option * term'
  | Import  of ty enumerate * identifier * CORE_identifier.label enumerate

and name =
  | Local of CORE_identifier.label
  | External of CORE_identifier.t * CORE_identifier.label

and t = component list

and term =
  | Lit of literal
  | Template of template
  | Variable of name
  | Lam of label * ty option * term'
  | App of term' * term'
  | IApp of term' * term' list

and template = template_atom list

and template_atom =
  | Raw of string
  | Code of term

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

and type_variable = TVariable of string

deriving (Json)

type checkpoint = string deriving (Json)

let string_of_ty ty =
  let rec aux context (TApp (TVariable tcon, args)) =
    match tcon, args with
      | tcon, [] ->
        tcon
      | "->", [ity; oty] ->
        may_paren context tcon (
          aux `LeftOfArrow ity ^ " -> "
          ^ aux `RightOfArrow oty
        )
      | tcon, args ->
        tcon ^ paren (String.concat ", " (List.map (aux `AsArgument) args))
  and may_paren context tcon s =
    if (match context, tcon with
      | `LeftOfArrow, "->" -> true
      | _, _ -> false
    ) then paren s else s
  and paren s = "(" ^ s ^ ")"
  in
  aux `AsArgument ty

}}

let timestamp_of_sub cs rkey =
  let rec aux = function
    | [] -> []
    | Sub (r, ts) :: _ when r = rkey -> [ ts ]
    | _ :: qs -> aux qs
  in
  aux cs

{shared{

type questions_value = [
  | `OK of atomic_value list
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
| `UnboundVariable of CORE_description_CST.term' * name
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

let import_exercise = ref None

let set_import_exercise f = import_exercise := Some f

let do_import_exercise id = match !import_exercise with
  | None -> assert false
  | Some f -> f id

type filter = name -> bool

type origin =
  | This of identifier
  | That of identifier * filter

let string_of_name = function
  | Local l -> label_to_string l
  | External (id, l) -> string_of_identifier id ^ "/" ^ label_to_string l

let filter_from_enumerate predicate =
  let rec make = function
    | All -> fun _ -> true
    | Insert xs -> fun id -> List.exists (predicate id) xs
    | Remove xs -> fun id -> not (List.exists (predicate id) xs)
    | Union es -> fun id -> List.for_all (fun a -> make a id) es
  in
  make

(* FIXME: The following implementation is probably broken.
   Indeed, we should do a dependency analysis to compute the
   transitive closure of imported names... *)
let do_imports (this : identifier) ?typeof p =
  let rec do_imports origin = function
    | Sub (e, _) ->
      lwt source = do_import_exercise e in
      do_imports' (That (e, fun _ -> true)) source

    | Import (tys, e, ls) ->
      let filter =
        match typeof with
          | None -> fun _ -> true
          | Some typeof ->
            let fls = filter_from_enumerate ( = ) ls in
            let filter_by_type =
              filter_from_enumerate (fun id t -> typeof id = t) tys
            in
            fun n ->
              let filter_by_name = function
                | Local l -> true
                | External (_, l) -> fls l
              in
              let rn = filter_by_name n in
              let rt = filter_by_type n in
              rn && rt
      in
      lwt source = do_import_exercise e in
      do_imports' (That (e, filter)) source

    | Binding (None, ty, t) ->
      (* FIXME: Maybe we should filter these if they come
         from an external. *)
      return [Binding (None, ty, t)]

    | (Binding (Some ((External (_, _)) as name), _, _) as b) ->
      (** The only bindings that already are external are the ones
          coming from an external source or that are already imported. *)
      begin match origin with
        | This _ -> assert false
        | That (_, filter) ->
          if filter name then return [b] else return []
      end

    | Binding (Some (Local l as name), ty, t) ->
      begin match origin with
        | This _ ->
          return [Binding (Some name, ty, t)]
        | That (e, filter) ->
          let name = External (e, l) in
          if filter name then
            return [Binding (Some name, ty, t)]
          else
            return []
      end

  and do_imports' (origin : origin) p =
    lwt ss = Lwt_list.map_s (do_imports origin) p in
    return (List.flatten ss)
  in
  do_imports' (This this) p


module TypeCheck = struct

  (** Invariant: bindings must be distinct. *)
  type environment = (name * ty) list

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
      "paragraph"; "statement"; "bold"; "italic"; "list"; "enumerate";
      "item"; "latex"; "ilatex"
    ]

  let statement_constructors_2 = List.iter
    (fun c -> primitive c (
      ttemplate statement --> statement
    ))
    [
      "section"; "subsection"
    ]

  let _ =
    primitive "checkpoint" ((unit --> unit) --> context);
    primitive "answer_in_file" (ttemplate string --> unit);
    primitive "answer_values_of" (ttemplate string --> unit);
    primitive "mark_using" (ttemplate string --> unit);
    primitive "mark_using_expected_values" (ttemplate string --> unit);
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
          check_term (bind (Local x) ity e) t (Some oty)
        | _, _ ->
          let ity = infer_term e t.source t.term in
          if not (compatible ty ity) then
            eraise (`TypeError (t.source, ty, ity));
          ity

  and infer_term e source = function
    | Lit l -> literal l
    | Variable x -> variable e source x
    | Template t -> begin match template e source t with
        | None -> ttemplate string
        | Some ty -> ty
    end
    | Lam (x, xty, t) -> lambda e source x t xty
    | App (a, b) -> app e source a b
    | _ -> assert false (* FIXME *)

  and template e source = function
    | [] -> None
    | [Code t] -> Some (ttemplate (infer_term e source t))
    | Raw _ :: ts -> template e source ts
    | Code t :: ts ->
      let ty = infer_term e source t in
      let rec flatten_ty = function
        | TApp (TVariable "template", [ ty ]) -> flatten_ty ty
        | ty -> ty
      in
      let ty = flatten_ty ty in
      match template e source ts with
        | Some (TApp (TVariable "template", [ ty' ])) ->
          if (compatible ty ty') then
            Some (ttemplate ty)
          else
            eraise (`TypeError (source, ttemplate ty, ttemplate ty'))
        | None -> Some (ttemplate ty)
        | Some ty' ->
          eraise (`TypeError (source, ttemplate ty, ty'))

  and literal = function
    | LInt _ -> int
    | LString _ -> string
    | LFloat _ -> float
    | LUnit -> unit

  and variable e source = function
    | (External _ ) as x ->
      begin try lookup x e with Not_found ->
        eraise (`UnboundVariable (source, x))
      end
    | (Local l) as x ->
      try lookup_primitive l with Not_found ->
        try lookup x e with Not_found ->
          eraise (`UnboundVariable (source, x))

  and app e source a b =
    match destruct_arrow (infer_term e a.source a.term) with
      | None -> eraise (`BadApplication source)
      | Some (ity, oty) -> ignore (check_term e b (Some ity)); oty

  and lambda e source x t = function
    | None -> eraise (`NeedAnnotation source)
    | Some ty -> ty --> (infer_term (bind (Local x) ty e) t.source t.term)

  let component e = function
    | Sub _ | Import _ ->
      assert false
    | Binding (None, ty, t) ->
      ignore (check_term e t ty);
      e
    | Binding (Some x, ty, t) ->
      bind x (check_term e t ty) e

  let program this p =
    lwt p = do_imports this p in
    return (List.fold_left component [] p)

end

module Eval = struct

  open Eliom_content
  open Html5.D
  open Html5

  type state = CORE_context.t

  type value =
    | VContext of CORE_context.t
    | VTemplate of value list
    | VInt of int
    | VFloat of float
    | VString of string
    | VStatement of string
    | VUnit
    | VClosure of environment * label * term
    | VPrimitive of (state -> value -> (state * value) Lwt.t)
    | VSource of string * string

  and environment = (name * value) list

  let rec eval_template join make t =
    join (List.map (eval_template_atom_value join make) t)

  and eval_template_atom_value join make = function
    | VTemplate t -> eval_template join make t
    | v -> make v

  let string_of_string_template =
    eval_template
      (String.concat "")
      (function VString s -> s | _ -> eraise `EvalError)

  let string_of_statement_template =
    eval_template
      (String.concat "")
      (function
        | VStatement s -> s
        | VString s -> s
        | _ -> eraise `EvalError)

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
      apply block state VUnit
      >>= fun (s, _) -> return (s, VContext s)
    );

    let as_string_list = function
      | VTemplate t ->
        eval_template ( List.flatten ) (function
          | VString "" -> []
          | VString s -> [s]
          | _ -> eraise `EvalError
        ) t
      | VString "" ->
        []
      | VString s ->
        [s]
      | _ -> eraise `EvalError
    in
    let as_string = function
      | VTemplate t -> string_of_statement_template t
      | VString s -> s
      | _ -> eraise `EvalError
    in
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
    let html_constructor2 (s, b1, c1, b2, c2) =
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
      "bold", "span", Some "bold";
      "italic", "span", Some "italic";
      "list", "ul", None;
      "enumerate", "ol", None;
      "item", "li", None;
      "section", "h1", None;
      "subsection", "h2", None
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

    stateful "mark_using" (fun v ->
      let s = as_string v in
      return (CORE_context.command s)
    );

    stateful "mark_using_expected_values" (fun v ->
      let s = as_string_list v in
      return (CORE_context.expected_values s)
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
    | Local x ->
      begin try_lwt
        return (s, VPrimitive (Hashtbl.find primitives x))
      with Not_found ->
        try_lwt
          return (s, List.assoc (Local x) e)
        with Not_found -> eraise `EvalError
      end
    | x ->
        try_lwt
          return (s, List.assoc x e)
        with Not_found -> eraise `EvalError

  and literal = function
    | LInt x -> VInt x
    | LString s -> VString s
    | LFloat f -> VFloat f
    | LUnit -> VUnit

  and closure s env x t =
    return (s, VClosure (env, x, t.term))

  and apply f s v : (state * value) Lwt.t =
    match f with
      | VClosure (e, x, t) -> term s ((Local x, v) :: e) t
      | VPrimitive p -> p s v
      | _ -> eraise `EvalError (* FIXME: Handle error. *)

  and template s e = function
    | [] ->
      return (s, [])
    | a :: t ->
      lwt (s, av) = template_atom s e a in
      lwt (s, tv) = template s e t in
      return (s, av :: tv)

  and template_atom s e = function
    | Raw sl ->
      return (s, VString sl)
    | Code t ->
      lwt (s, v) = term s e t in
      return (s, v)

  and term s e = function
    | Lit l -> return (s, literal l)
    | Template t -> lwt s, tv = template s e t in return (s, VTemplate tv)
    | Variable x -> variable s e x
    | Lam (x, _, t) -> closure s e x t
    | App (a, b) ->
      lwt s, a = term' s e a in
      lwt s, b = term' s e b in
      apply a s b
    | IApp _ -> assert false

  and term' s e t = term s e t.term

  let _ = make_primitive ()

  let name_to_local_string = function
    | Local l -> label_to_string l
    | External (id, l) ->
      let sid = string_of_identifier id in
      Str.(global_replace (regexp "/") "_" sid) ^ "_" ^ label_to_string l

  let program this tenv p =
    let fresh =
      let c = ref 0 in fun () ->
        incr c;
        Local (CORE_identifier.label ("_" ^ string_of_int !c))
    in
    lwt p = do_imports this ~typeof:(fun n -> TypeCheck.lookup n tenv) p in
    let rec component (s, e) = function
      | Sub _ | Import _ -> assert false
      | Binding (None, _, t) ->
        term' s e t >>= fun (s, v) -> return (s, (fresh (), v) :: e)
      | Binding (Some x, _, t) ->
        term' s e t >>= fun (s, v) -> return (s, (x, v) :: e)
    in
    lwt _, e = Lwt_list.fold_left_s component (CORE_context.empty, []) p in
    return (filter_map (fun x -> function
      | VStatement s -> Some (Statement s)
      | VContext c -> Some (CheckpointContext (name_to_local_string x, c))
      | VSource (s, c) -> Some (Source (s, c))
      | _ -> None
      ) (List.rev e)
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
    `UnboundVariable (start_of t, string_of_name n)
  | `BadApplication t ->
    `BadApplication (start_of t)
  | `EvalError ->
    `EvalError
)

(** A well-typed exercise description evaluates into a value. *)
let eval this p : questions_value Lwt.t =
  try_lwt
    lwt tenv = TypeCheck.program this p in
    lwt v = Eval.program this tenv p in
    return (`OK v)
  with Error e ->
    return (`KO (convert_to_string_error e))

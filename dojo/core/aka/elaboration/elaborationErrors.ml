open ElaborationExceptions
open Name
open Errors

let string_of_type ty      = ASTio.(XAST.(to_string pprint_ml_type ty))
let string_of_kind k       = ASTio.(XAST.(to_string pprint_ml_kind k))

let handle_error f =
  try
    f ()
  with
    | CannotElaborateDictionary (pos, ty) ->
      fatal' pos (Printf.sprintf "  Cannot elaborate a dictionary of type %s."
                   (string_of_type ty))

    | UnboundIdentifier (pos, Name x) ->
      fatal' pos (Printf.sprintf
                   "  Identifier `%s' is unbound."
                   x)

    | UnboundTypeVariable (pos, TName x) ->
      fatal' pos (Printf.sprintf
                   "  Type `%s' is unbound."
                   x)

    | UnboundClass (pos, TName x) ->
      fatal' pos (Printf.sprintf
                   "  Class `%s' is unbound."
                   x)

    | UnboundLabel (pos, LName x) ->
      fatal' pos (Printf.sprintf
                   "  Label `%s' is unbound."
                   x)

    | OverlappingInstances (pos, TName k) ->
      fatal' pos (Printf.sprintf
                   "  This instance of class `%s' overlaps with another one."
                   k)

    | AlreadyDefinedClass (pos, TName x) ->
      fatal' pos (Printf.sprintf
                   "  Class `%s' is already defined."
                   x)

    | NonLinearPattern pos ->
      fatal' pos "  This pattern is not linear."

    | InvalidTypeApplication pos ->
      fatal' pos "  Invalid type application."

    | IncompatibleTypes (pos, ty1, ty2) ->
      fatal' pos (Printf.sprintf
                    "  The following types are incompatible:\n  %s\n  %s"
                    (string_of_type ty1)
                    (string_of_type ty2))

    | IncompatibleKinds (pos, k1, k2) ->
      fatal' pos (Printf.sprintf
                    "  The following kinds are incompatible:\n  %s\n  %s"
                    (string_of_kind k1)
                    (string_of_kind k2))

    | RecordExpected (pos, ty) ->
      fatal' pos (Printf.sprintf
                    "  The following type is not a record:\n  %s\n"
                    (string_of_type ty))

    | ApplicationToNonFunctional pos ->
      fatal' pos "  The left-hand side of this application is not a function."

    | PatternsMustBindSameVariables pos ->
      fatal' pos "  The following subpatterns must bind the same variables."

    | InvalidDataConstructorApplication pos ->
      fatal' pos "  Bad number of arguments."

    | IllKindedType pos ->
      fatal' pos "  Ill-kinded type."

    | ValueRestriction pos ->
      fatal' pos "  Only value forms can be polymorphic."

    | InvalidOverloading pos ->
      fatal' pos "  Invalid overloading."

    | InvalidNumberOfTypeAbstraction pos ->
      fatal' pos "  Invalid number of type abstractions."

    | TheseTwoClassesMustNotBeInTheSameContext (pos, TName k1, TName k2) ->
      fatal' pos (Printf.sprintf
                    "  The classes `%s' and '%s' cannot be used \
                    with the same parameter in a context."
                    k1 k2)

    | OnlyLetsCanIntroduceTypeAbstraction pos ->
      fatal' pos "  Only let bindings can introduce type abstractions."

    | SameNameInTypeAbstractionAndScheme pos ->
      fatal' pos "  The same names must be used to denote type parameters \n  \
                    in the type scheme and in the type abstractions of \n  \
                    let bindings."

    | LabelAlreadyTaken (pos, LName l) ->
      fatal' pos (Printf.sprintf
                    "  The label `%s' is already used by another record type."
                    l)

    | LabelDoesNotBelong (pos, LName l, TName ir, TName xr) ->
      fatal' pos (Printf.sprintf
                    "  The label `%s' belongs to `%s', not `%s'."
                    l xr ir)

    | InvalidRecordInstantiation pos ->
      fatal' pos "  Invalid record instantiation."

    | InvalidRecordConstruction pos ->
      fatal' pos "  Invalid record construction (not the right set of fields)."

    | MultipleLabels (pos, LName l) ->
      fatal' pos (Printf.sprintf "  Multiple definitions of label `%s'." l)

    | OverloadedSymbolCannotBeBound (pos, Name x) ->
      fatal' pos (Printf.sprintf
                   "  Identifier `%s' cannot be both overloaded and let-bound."
                   x)

(** Exceptions for type checking of {!XAST} and its elaboration in ML. *)
open Positions
open Name

(** [UnboundIdentifier] is raised if an identifier is unbound. *)
exception UnboundIdentifier of position * name

(** [UnboundTypeVariable] is raised if a type identifier is unbound. *)
exception UnboundTypeVariable of position * tname

(** [UnboundClass] is raised if a class identifier is unbound. *)
exception UnboundClass of position * tname

(** [UnboundLabel] is raised if a label is unbound. *)
exception UnboundLabel of position * lname

(** [MultipleLabels] is raised if a label is defined several
    times in a record. *)
exception MultipleLabels of position * lname

(** [AlreadyDefinedClass] is raised if a class is defined twice. *)
exception AlreadyDefinedClass of position * tname

(** [InvalidTypeApplication] is raised if an incorrect number of
    types is applied to a polymorphic term. *)
exception InvalidTypeApplication of position

(** [InvalidDataConstructorApplication] is raised when an incorrect
    number of arguments is passed to a data constructor. *)
exception InvalidDataConstructorApplication of position

(** [PatternsMustBindSameVariables] is raised when two subpatterns
    bind different variables, when they should not. *)
exception PatternsMustBindSameVariables of position

(** [CannotElaborateDictionary] is raised when a dictionary of some
    given type cannot be elaborated from the context. *)
exception CannotElaborateDictionary of position * Types.t

(** [NonLinearPattern] is raised when a pattern does not respect
    the linearity condition of ML patterns. *)
exception NonLinearPattern of position

(** [IncompatibleTypes] is raised when two types are incompatible,
    although they should. *)
exception IncompatibleTypes of position * Types.t * Types.t

(** [IncompatibleKinds] is raised when two kinds are incompatible,
    although they should. *)
exception IncompatibleKinds of position * Types.kind * Types.kind

(** [IllKindedType] is raised when a type is ill-formed. *)
exception IllKindedType of position

(** [RecordExpected] is raised when a record type is expected
    but another type is inferred. *)
exception RecordExpected of position * Types.t

(** [ApplicationToNonFunctional] is raised when the left hand side
    of an application is not a function. *)
exception ApplicationToNonFunctional of position

(** [ValueRestriction] is raised when a let-binding do not respect
    the so-called value restriction. *)
exception ValueRestriction of position

(** [InvalidOverloading] is raised when a symbol cannot be overloaded
    because its definition does not respect the restrictions of the
    specification. *)
exception InvalidOverloading of position

(** [InvalidNumberOfTypeAbstraction] is raised when the number of
    type abstractions in a let-binding do not match the number of
    type parameters of its type scheme. *)
exception InvalidNumberOfTypeAbstraction of position

(** [TheseTwoClassesMustNotBeInTheSameContext] is raised when
    two class predicates prevent a typing context to be canonical. *)
exception TheseTwoClassesMustNotBeInTheSameContext of position * tname * tname

(** [OverlappingInstances] is raised when two instances' indices share
    the same head symbol. *)
exception OverlappingInstances of position * tname

(** [OnlyLetsCanIntroduceTypeAbstraction] is raised when a type abstraction
    is used deep in a term, when it should only appear immediatly under
    let-bindings. *)
exception OnlyLetsCanIntroduceTypeAbstraction of position

(** [SameNameInTypeAbstractionAndScheme] is raised when the names
    introduced by the type abstractions of a let-binding do not
    match the names of the type parameters of its type scheme. *)
exception SameNameInTypeAbstractionAndScheme of position

(** [LabelAlreadyTaken] is raised when a record type declaration
    uses a label already used by another record type declaration. *)
exception LabelAlreadyTaken of position * lname

(** [LabelDoesNotBelong] is raised when a label is used with a
    record type that does not contain it. *)
exception LabelDoesNotBelong of position * lname * tname * tname

(** [InvalidRecordInstantiation] is raised when a record constructor
    is applied to an incorrect number of types. *)
exception InvalidRecordInstantiation of position

(** [InvalidRecordConstruction] is raised when a record is not built
    using the right labels. *)
exception InvalidRecordConstruction of position

(** [OverloadedSymbolCannotBeBound] is raised when an overloaded
    symbol is introduced by a let. *)
exception OverloadedSymbolCannotBeBound of Positions.position * name

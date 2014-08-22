(** Parsing and pretty-printing of AST for ML variants. *)

module type S =
sig
  module AST : AST.GenericS

  (** [pprint_program ast] prints [ast] in document that is syntactically
      correct. *)
  val pprint_program : AST.program -> PPrintEngine.document

  (** [pprint_program_in_caml ast] translates [ast] into an ocaml program. *)
  val pprint_program_in_ocaml : AST.program -> PPrintEngine.document

  (** [pprint_expression e] prints [e]. *)
  val pprint_expression : AST.expression -> PPrint.document

  (** [pprint_ml_type ty] prints [ty]. *)
  val pprint_ml_type : Types.t -> PPrint.document

  (** [pprint_ml_kind k] prints [k]. *)
  val pprint_ml_kind : Types.kind -> PPrint.document
end

module Make : functor (AST : AST.GenericS) -> S with module AST = AST

module IAST : S with module AST = IAST
module XAST : S with module AST = XAST

(** An extension to PPrint. *)
val to_string : ('a -> PPrintEngine.ToBuffer.document) -> 'a -> string

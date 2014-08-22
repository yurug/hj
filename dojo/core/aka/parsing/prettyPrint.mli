(** A generic pretty printer. *)

module Make :
  functor (GAST : AST.GenericS) ->
    sig
      val ml_type : Types.t -> PPrint.document
      val ml_kind : Types.kind -> PPrint.document
      val expression : GAST.expression -> PPrint.document
      val program : bool -> GAST.program -> PPrintEngine.document
    end

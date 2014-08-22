(** Parsing and pretty-printing of AST for ML variants. *)

module type S =
sig
  module AST : AST.GenericS
  val pprint_program : AST.program -> PPrintEngine.document
  val pprint_program_in_ocaml : AST.program -> PPrintEngine.document
  val pprint_expression : AST.expression -> PPrint.document
  val pprint_ml_type : Types.t -> PPrint.document
  val pprint_ml_kind : Types.kind -> PPrint.document
end

module Make (AST : AST.GenericS) = struct

  module AST = AST
  module O = PrettyPrint.Make (AST)

  let pprint_program ast =
    O.program false ast

  let pprint_program_in_ocaml ast =
    O.program true ast

  let pprint_expression ast =
    O.expression ast

  let pprint_ml_type ast =
    O.ml_type ast

  let pprint_ml_kind ast =
    O.ml_kind ast

end

module IAST = Make (IAST)
module XAST = Make (XAST)

let to_string pprint ast =
  let b = Buffer.create 13 in
  PPrintEngine.ToBuffer.pretty 0.8 100 b (pprint ast);
  Buffer.contents b

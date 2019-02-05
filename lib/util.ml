open Ppxlib

let suffix_from_type_name = function
  | "t" -> ""
  | s -> "_" ^ s

module Expr = struct
  let var ~loc var_name = Ast_builder.Default.pexp_ident ~loc {txt = Lident var_name; loc}
  let ctr ~loc ~ctr_name expr =
    Ast_builder.Default.pexp_construct ~loc {txt = Lident ctr_name; loc} expr
end

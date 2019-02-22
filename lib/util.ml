open Ppxlib

let suffix_from_type_name = function
  | "t" -> ""
  | s -> "_" ^ s

let constr_from_type_param ~loc (core_type, _variance) =
  {core_type with ptyp_loc = loc; ptyp_attributes = []}

let core_type_from_type_decl ~loc {ptype_name; ptype_params; _} =
  let constr = List.map (constr_from_type_param ~loc) ptype_params in
  let type_lident = {txt = Lident ptype_name.txt; loc} in
  Ast_builder.Default.ptyp_constr ~loc type_lident constr

module Expr = struct
  let var ~loc var_name = Ast_builder.Default.pexp_ident ~loc {txt = Lident var_name; loc}
  let constructor ~loc ~constructor_name expr =
    Ast_builder.Default.pexp_construct ~loc {txt = Lident constructor_name; loc} expr
end

module List_ = struct
  exception Exit

  (* Thanks c-cube's containers *)
  let all_ok l =
    let err = ref None in
    try Ok (List.map (function Ok x -> x | Error e -> err := Some e; raise Exit) l)
    with Exit ->
    match !err with
    | Some e -> Error e
    | None -> assert false
end

module Result_ = struct
  let (>|=) res f =
    match res with
    | Ok x -> Ok (f x)
    | Error _ as err -> err
end

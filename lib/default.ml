open Ppxlib

let _name_from_type_name type_name =
  Printf.sprintf "default%s" @@ Util.suffix_from_type_name type_name

let expr_from_lident ~loc {txt; loc = err_loc} =
  match txt with
  | Lident name ->
    Ast_builder.Default.pexp_ident ~loc {txt = Lident (_name_from_type_name name); loc}
  | Ldot (lident, last) ->
    Ast_builder.Default.pexp_ident ~loc {txt = Ldot (lident, _name_from_type_name last); loc}
  | Lapply _ -> Raise.errorf ~loc:err_loc "unhandled longident"

let rec expr_from_core_type ~loc {ptyp_desc; ptyp_loc; _} =
  match ptyp_desc with
  | Ptyp_constr ({txt = Lident "bool"; _}, _) -> [%expr false]
  | Ptyp_constr ({txt = Lident "int"; _}, _) -> [%expr 0]
  | Ptyp_constr ({txt = Lident "int32" | Ldot (Lident "Int32", "t"); _}, _) -> [%expr 0l]
  | Ptyp_constr ({txt = Lident "int64" | Ldot (Lident "Int64", "t"); _}, _) -> [%expr 0L]
  | Ptyp_constr ({txt = Lident "nativeint" | Ldot (Lident "Nativeint", "t"); _}, _) -> [%expr 0n]
  | Ptyp_constr ({txt = Lident "float" | Ldot (Lident "Float", "t"); _}, _) -> [%expr 0.]
  | Ptyp_constr ({txt = Lident "char" | Ldot (Lident "Char", "t"); _}, _) -> [%expr '\x00']
  | Ptyp_constr ({txt = Lident "string" | Ldot (Lident "String", "t"); _}, _) -> [%expr ""]
  | Ptyp_constr ({txt = Lident "option"; _}, _) -> [%expr None]
  | Ptyp_constr ({txt = Lident "list"; _}, _) -> [%expr []]
  | Ptyp_constr ({txt = Lident "array"; _}, _) -> [%expr [||]]
  | Ptyp_constr (lident, _) -> expr_from_lident ~loc lident
  | Ptyp_tuple types ->
    let expr_list = List.map (expr_from_core_type ~loc) types in
    Ast_builder.Default.pexp_tuple ~loc expr_list
  | Ptyp_var _ -> Raise.errorf ~loc:ptyp_loc "can't derive default for unspecified type" 
  | _ -> Raise.errorf ~loc:ptyp_loc "can't derive default value from this type"

module Str = struct
  let value_expr_from_manifest ~ptype_loc ~loc manifest =
    match manifest with
    | None ->
      Raise.Default.errorf
        ~loc:ptype_loc
        "can't derive default for an abstract type without a manifest"
    | Some typ -> expr_from_core_type ~loc typ

  let value_pat_from_name ~loc type_name =
    let name = _name_from_type_name type_name in
    Ast_builder.Default.ppat_var ~loc {txt = name; loc}

  let from_td ~loc {ptype_name; ptype_kind; ptype_manifest; ptype_loc; _} =
    let expr =
      match ptype_kind with
      | Ptype_abstract -> value_expr_from_manifest ~ptype_loc ~loc ptype_manifest
      | Ptype_variant _
      | Ptype_record _
      | Ptype_open -> Raise.Default.errorf ~loc:ptype_loc "unhandled type kind"
    in
    let pat = value_pat_from_name ~loc ptype_name.txt in
    let value_binding = Ast_builder.Default.value_binding ~loc ~pat ~expr in
    Ast_builder.Default.pstr_value ~loc Nonrecursive [value_binding]

  let from_type_decl ~loc ~path:_ (_rec_flag, tds) = List.map (from_td ~loc) tds
end

module Sig = struct
  let constr_from_type_param ~loc (core_type, _variance) =
    {core_type with ptyp_loc = loc; ptyp_attributes = []}

  let from_td ~loc {ptype_name; ptype_params; _} =
    let name = {txt = _name_from_type_name ptype_name.txt; loc} in
    let constr = List.map (constr_from_type_param ~loc) ptype_params in
    let type_lident = {txt = Lident ptype_name.txt; loc} in
    let type_ = Ast_builder.Default.ptyp_constr ~loc type_lident constr in
    let value_description = Ast_builder.Default.value_description ~loc ~name ~type_ ~prim:[] in
    Ast_builder.Default.psig_value ~loc value_description

  let from_type_decl ~loc ~path:_ (_rec_flag, tds) = List.map (from_td ~loc) tds
end

let from_str_type_decl = Deriving.Generator.make_noarg Str.from_type_decl

let from_sig_type_decl = Deriving.Generator.make_noarg Sig.from_type_decl

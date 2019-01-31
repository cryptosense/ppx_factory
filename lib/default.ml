open Ppxlib

let _name_from_type_name type_name =
  Printf.sprintf "default%s" @@ Util.suffix_from_type_name type_name

module Str = struct
  let value_expr_from_core_type ~loc {ptyp_desc; ptyp_loc; _} =
    match ptyp_desc with
    | Ptyp_constr ({txt = Lident "int"; _}, _) -> [%expr 0]
    | Ptyp_constr ({txt = Lident "float"; _}, _) -> [%expr 0.]
    | Ptyp_constr ({txt = Lident "char"; _}, _) -> [%expr '\x00']
    | Ptyp_constr ({txt = Lident "string"; _}, _) -> [%expr ""]
    | Ptyp_constr ({txt = Lident "option"; _}, _) -> [%expr None]
    | Ptyp_constr ({txt = Lident "list"; _}, _) -> [%expr []]
    | _ -> Location.raise_errorf ~loc:ptyp_loc "ppx_factory: default: unhandled type"

  let value_expr_from_manifest ~ptype_loc ~loc manifest =
    match manifest with
    | None ->
      Location.raise_errorf
        ~loc:ptype_loc
        "ppx_factory: default: can't derive default for an abstract type without a manifest"
    | Some typ -> value_expr_from_core_type ~loc typ

  let value_pat_from_name ~loc type_name =
    let name = _name_from_type_name type_name in
    Ast_builder.Default.ppat_var ~loc {txt = name; loc}

  let from_td ~loc {ptype_name; ptype_kind; ptype_manifest; ptype_loc; _} =
    let expr =
      match ptype_kind with
      | Ptype_abstract -> value_expr_from_manifest ~ptype_loc ~loc ptype_manifest
      | Ptype_variant _
      | Ptype_record _
      | Ptype_open ->
        Location.raise_errorf ~loc:ptype_loc "ppx_factory: default: unhandled type kind"
    in
    let pat = value_pat_from_name ~loc ptype_name.txt in
    let value_binding = Ast_builder.Default.value_binding ~loc ~pat ~expr in
    Ast_builder.Default.pstr_value ~loc Nonrecursive [value_binding]


  let from_type_decl ~loc ~path:_ (_rec_flag, tds) =
    List.rev (List.rev_map (from_td ~loc) tds)
end

let from_str_type_decl = Deriving.Generator.make_noarg Str.from_type_decl

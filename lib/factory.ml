open Ppxlib

let _name_from_type_name type_name =
  Printf.sprintf "factory%s" @@ Util.suffix_from_type_name type_name

module Str = struct
  let field_name_and_default ~loc {pld_name; pld_type; _} =
    let name = pld_name.txt in
    let default = Default.expr_from_core_type ~loc pld_type in
    (name, default)

  let fixed_field_binding ~loc name =
    let lident = {txt = Lident name; loc} in
    (lident, Ast_builder.Default.pexp_ident ~loc lident)

  let fun_expr_from_labels ~loc labels =
    let names_and_defaults = List.map (field_name_and_default ~loc) labels in
    let fields_bindings =
      List.map (fun (name, _) -> fixed_field_binding ~loc name) names_and_defaults
    in
    let return_expr = Ast_builder.Default.pexp_record ~loc fields_bindings None in
    List.fold_right
      ( fun (name, default) acc ->
          let arg_label = Optional name in
          let default_value = Some default in
          let pattern = Ast_builder.Default.ppat_var ~loc {txt = name; loc} in
          Ast_builder.Default.pexp_fun ~loc arg_label default_value pattern acc
      )
      names_and_defaults
      [%expr fun () -> [%e return_expr]]

  let from_record ~loc ~type_name ~labels =
    let factory_name = _name_from_type_name type_name in
    let pat = Ast_builder.Default.ppat_var ~loc {txt = factory_name; loc} in
    let expr = fun_expr_from_labels ~loc labels in
    let value_binding = Ast_builder.Default.value_binding ~loc ~pat ~expr in
    [Ast_builder.Default.pstr_value ~loc Nonrecursive [value_binding]]

  let from_td ~loc {ptype_name = {txt = type_name; _}; ptype_kind; ptype_loc; _} =
    match ptype_kind with
    | Ptype_record labels -> from_record ~loc ~type_name ~labels
    | Ptype_variant _ -> Raise.Factory.errorf ~loc:ptype_loc "can't derive from variant type yet"
    | Ptype_abstract -> Raise.Factory.unhandled_type_kind ~loc "abstract"
    | Ptype_open -> Raise.Factory.unhandled_type_kind ~loc "open"

  let from_type_decl ~loc ~path:_ (_rec_flag, tds) =
    List.flatten @@ List.map (from_td ~loc) tds
end

module Sig = struct
  let from_type_decl ~loc:_ ~path:_ (_rec_flag, _tds) =
    []
end

let from_str_type_decl =
  Deriving.Generator.make_noarg Str.from_type_decl

let from_sig_type_decl =
  Deriving.Generator.make_noarg Sig.from_type_decl

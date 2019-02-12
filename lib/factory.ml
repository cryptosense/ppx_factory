open Ppxlib

let _name_from_type_name type_name =
  Printf.sprintf "factory%s" @@ Util.suffix_from_type_name type_name

let _name_from_type_and_ctr_name ~type_name ~ctr_name =
  let base_factory_name = _name_from_type_name type_name in
  Printf.sprintf "%s_%s" base_factory_name (String.lowercase_ascii ctr_name)

module Str = struct
  let factory_fun_expr ~loc ~return_expr ~arg_names ~defaults =
    List.fold_right2
      ( fun name default acc ->
          let arg_label = Optional name in
          let pattern = Ast_builder.Default.ppat_var ~loc {txt = name; loc} in
          Ast_builder.Default.pexp_fun ~loc arg_label default pattern acc
      )
      arg_names
      defaults
      [%expr fun () -> [%e return_expr]]

  let default_arg_from_core_type ~loc core_type =
    match core_type with
    | [%type: [%t? _] option] -> None
    | _ -> Some (Default.expr_from_core_type ~loc core_type)

  let arg_names_from_labels labels =
    List.map (fun {pld_name; _} -> pld_name.txt) labels

  let defaults_from_label_decl ~loc labels =
    List.map (fun {pld_type; _} -> default_arg_from_core_type ~loc pld_type) labels

  let fixed_field_binding ~loc name =
    let lident = {txt = Lident name; loc} in
    (lident, Util.Expr.var ~loc name)

  let fun_expr_from_labels ~loc ?ctr_name labels =
    let arg_names = arg_names_from_labels labels in
    let fields_bindings = List.map (fixed_field_binding ~loc) arg_names in
    let record_expr = Ast_builder.Default.pexp_record ~loc fields_bindings None in
    let return_expr =
      match ctr_name with
      | None -> record_expr
      | Some ctr_name -> Util.Expr.ctr ~loc ~ctr_name (Some record_expr)
    in
    let defaults = defaults_from_label_decl ~loc labels in
    factory_fun_expr ~loc ~return_expr ~arg_names ~defaults

  let from_labels ~loc ~factory_name ?ctr_name labels =
    let pat = Ast_builder.Default.ppat_var ~loc {txt = factory_name; loc} in
    let expr = fun_expr_from_labels ~loc ?ctr_name labels in
    let value_binding = Ast_builder.Default.value_binding ~loc ~pat ~expr in
    Ast_builder.Default.pstr_value ~loc Nonrecursive [value_binding]

  let from_record ~loc ~type_name ~labels =
    let factory_name = _name_from_type_name type_name in
    [from_labels ~loc ~factory_name labels]

  let arg_names_from_tuple types =
    List.mapi (fun i _ -> Printf.sprintf "tup%d" i) types

  let defaults_from_tuple ~loc types =
    List.map (fun core_type -> default_arg_from_core_type ~loc core_type) types

  let fun_expr_from_ctr_tuple ~loc ~ctr_name types =
    let arg_names = arg_names_from_tuple types in
    let tuple_bindings = List.map (Util.Expr.var ~loc) arg_names in
    let ctr_arg_expr =
      match tuple_bindings with
      | [] -> None
      | [expr] -> Some expr
      | _ -> Some (Ast_builder.Default.pexp_tuple ~loc tuple_bindings)
    in
    let return_expr = Util.Expr.ctr ~loc ~ctr_name ctr_arg_expr in
    let defaults = defaults_from_tuple ~loc types in
    factory_fun_expr ~loc ~return_expr ~arg_names ~defaults

  let from_ctr_tuple ~loc ~factory_name ~ctr_name types =
    let pat = Ast_builder.Default.ppat_var ~loc {txt = factory_name; loc} in
    let expr = fun_expr_from_ctr_tuple ~loc ~ctr_name types in
    let value_binding = Ast_builder.Default.value_binding ~loc ~pat ~expr in
    Ast_builder.Default.pstr_value ~loc Nonrecursive [value_binding]

  let from_ctr_record ~loc ~factory_name ~ctr_name labels =
    from_labels ~loc ~factory_name ~ctr_name labels

  let from_constructor ~loc ~type_name {pcd_name = {txt = ctr_name; _}; pcd_args; _} =
    let factory_name = _name_from_type_and_ctr_name ~type_name ~ctr_name in 
    match pcd_args with
    | Pcstr_tuple types -> from_ctr_tuple ~loc ~factory_name ~ctr_name types
    | Pcstr_record labels -> from_ctr_record ~loc ~factory_name ~ctr_name labels

  let from_td ~loc {ptype_name = {txt = type_name; _}; ptype_kind; _} =
    match ptype_kind with
    | Ptype_record labels -> from_record ~loc ~type_name ~labels
    | Ptype_variant constructors -> List.map (from_constructor ~type_name ~loc) constructors
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

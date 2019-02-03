open Ppxlib

val from_str_type_decl : (structure, rec_flag * type_declaration list) Deriving.Generator.t

val from_sig_type_decl : (signature, rec_flag * type_declaration list) Deriving.Generator.t

val _name_from_type_name : string -> string

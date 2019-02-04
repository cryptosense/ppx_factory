open Ppxlib

(** Structure generator *)
val from_str_type_decl : (structure, rec_flag * type_declaration list) Deriving.Generator.t

(** Signature generator *)
val from_sig_type_decl : (signature, rec_flag * type_declaration list) Deriving.Generator.t

(** Return the name of the factory function derived from a type with the given name. *)
val _name_from_type_name : string -> string

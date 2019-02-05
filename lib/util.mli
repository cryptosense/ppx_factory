open Ppxlib

(** Return the suffix to apply to a derived value name, based on the type name *)
val suffix_from_type_name : string -> string

module Expr : sig
  (** Return the expression corresponding to the given variable name *)
  val var : loc: Location.t -> string -> expression

  (** Return the contructor expression with the given constructor name and argument expression *)
  val ctr : loc: Location.t -> ctr_name: string -> expression option -> expression
end

open Ppxlib

(** Return the suffix to apply to a derived value name, based on the type name *)
val suffix_from_type_name : string -> string

(** Return the core type describing the type declared in the given declaration.
    E.g. will return [[%type: 'a t]] for [type 'a t = A of int | B of 'a].
*)
val core_type_from_type_decl : loc: Location.t -> type_declaration -> core_type

module Expr : sig
  (** Return the expression corresponding to the given variable name *)
  val var : loc: Location.t -> string -> expression

  (** Return the contructor expression with the given constructor name and argument expression *)
  val constructor : loc: Location.t -> constructor_name: string -> expression option -> expression
end

module List_ : sig
  (** [all_ok l] return [l'] if all elements of [l] are [Ok _] or the first encountered
      error otherwise.
  *)
  val all_ok : ('a, 'b) result list -> ('a list, 'b) result
end

module Result_ : sig
  val (>|=) : ('a, 'err) result -> ('a -> 'b) -> ('b, 'err) result
end

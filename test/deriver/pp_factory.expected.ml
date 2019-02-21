module A = struct module B = struct type t = char
                                    let default = 'c' end end
module Types :
  sig
    type simple_record =
      {
      int_field: int ;
      string_field: string ;
      other_field: A.B.t }[@@deriving factory]
    val factory_simple_record :
      ?int_field:int ->
        ?string_field:string -> ?other_field:A.B.t -> unit -> simple_record
    type simple_variant =
      | A 
      | B of int 
      | C of int * string 
      | D of {
      int_field: int ;
      string_field: string } [@@deriving factory]
    val factory_simple_variant_a : unit -> simple_variant
    val factory_simple_variant_b : ?tup0:int -> unit -> simple_variant
    val factory_simple_variant_c :
      ?tup0:int -> ?tup1:string -> unit -> simple_variant
    val factory_simple_variant_d :
      ?int_field:int -> ?string_field:string -> unit -> simple_variant
    type record_with_options =
      {
      non_optional: int ;
      optional: int option ;
      nested: int option option }[@@deriving factory]
    val factory_record_with_options :
      ?non_optional:int ->
        ?optional:int -> ?nested:int option -> unit -> record_with_options
    type 'a parametrized = {
      param: 'a option ;
      non_paramed: string }[@@deriving factory]
    val factory_parametrized :
      ?param:'a -> ?non_paramed:string -> unit -> 'a parametrized
    type copied = simple_record =
      {
      int_field: int ;
      string_field: string ;
      other_field: A.B.t }[@@deriving factory]
    val factory_copied :
      ?int_field:int ->
        ?string_field:string -> ?other_field:A.B.t -> unit -> copied
  end =
  struct
    type simple_record =
      {
      int_field: int ;
      string_field: string ;
      other_field: A.B.t }[@@deriving factory]
    let factory_simple_record ?(int_field= 0)  ?(string_field= "") 
      ?(other_field= A.B.default)  () =
      { int_field; string_field; other_field }
    type simple_variant =
      | A 
      | B of int 
      | C of int * string 
      | D of {
      int_field: int ;
      string_field: string } [@@deriving factory]
    let factory_simple_variant_a () = A
    let factory_simple_variant_b ?(tup0= 0)  () = B tup0
    let factory_simple_variant_c ?(tup0= 0)  ?(tup1= "")  () = C (tup0, tup1)
    let factory_simple_variant_d ?(int_field= 0)  ?(string_field= "")  () =
      D { int_field; string_field }
    type record_with_options =
      {
      non_optional: int ;
      optional: int option ;
      nested: int option option }[@@deriving factory]
    let factory_record_with_options ?(non_optional= 0)  ?optional  ?nested 
      () = { non_optional; optional; nested }
    type 'a parametrized = {
      param: 'a option ;
      non_paramed: string }[@@deriving factory]
    let factory_parametrized ?param  ?(non_paramed= "")  () =
      { param; non_paramed }
    type copied = simple_record =
      {
      int_field: int ;
      string_field: string ;
      other_field: A.B.t }[@@deriving factory]
    let factory_copied ?(int_field= 0)  ?(string_field= "")  ?(other_field=
      A.B.default)  () = { int_field; string_field; other_field }
  end 

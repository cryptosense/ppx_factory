module A = struct
  module B = struct
    type t = char
    let default = 'c'
  end
end

type simple_record =
  { int_field : int
  ; string_field : string
  ; other_field : A.B.t
  }
[@@deriving factory]

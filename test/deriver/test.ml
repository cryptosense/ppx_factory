type _bool = bool [@@deriving default]
type _int = int [@@deriving default]
type _int32 = int32 [@@deriving default]
type int32_t = Int32.t [@@deriving default]
type _int64 = int64 [@@deriving default]
type int64_t = Int64.t [@@deriving default]
type _nativeint = nativeint [@@deriving default]
type nativeint_t = Nativeint.t [@@deriving default]
type _float = float [@@deriving default]
type float_t = Float.t [@@deriving default]
type _char = char [@@deriving default]
type char_t = Char.t [@@deriving default]
type _string = string [@@deriving default]
type string_t = String.t [@@deriving default]
type _option = int option [@@deriving default]
type _list = int list [@@deriving default]
type _array = int array [@@deriving default]

module type DEFAULT = sig
  type t
  [@@deriving default]
  type simple
  [@@deriving default]
  type with_manifest = int
  [@@deriving default]
  type private_ = private int
  [@@deriving default]
  type ('a, 'b) parametrized
  [@@deriving default]
  type variant = A of int | B of string
  [@@deriving default]
  type record = {a : int; b : string}
  [@@deriving default]
end

type t =
  { loc : Location.t
  ; msg : string
  }

let as_result ~loc ~msg = Error {loc; msg}

let raise_ {loc; msg} = Raise.errorf ~loc "%s" msg

let ok_or_raise = function
  | Ok x -> x
  | Error err -> raise_ err

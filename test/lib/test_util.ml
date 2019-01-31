open OUnit2

let test_suffix_from_type_name =
  let test ~input ~expected ctxt =
    let actual = Ppx_factory_lib.Util.suffix_from_type_name input in
    assert_equal ~ctxt ~cmp:[%eq: string] ~printer:[%show: string] expected actual
  in
  "suffix_from_type_name" >:::
  [ "Type t" >:: test ~input:"t" ~expected:""
  ; "Other type name" >:: test ~input:"a" ~expected:"_a"
  ; "Preserves leading underscores" >:: test ~input:"_a" ~expected:"__a"
  ]

let suite =
  "Util" >:::
  [ test_suffix_from_type_name
  ]

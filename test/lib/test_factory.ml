open OUnit2

let test__name_from_type_name =
  let test ~input ~expected ctxt =
    let actual = Ppx_factory_lib.Factory._name_from_type_name input in
    assert_equal ~ctxt ~cmp:[%eq: string] ~printer:[%show: string] expected actual
  in
  "_name_from_type_name" >:::
  [ "Is factory" >:: test ~input:"t" ~expected:"factory"
  ; "Uses right suffix" >:: test ~input:"a" ~expected:"factory_a"
  ]

let test__name_from_type_and_constructor_name =
  let test ~type_name ~constructor_name ~expected ctxt =
    let actual =
      Ppx_factory_lib.Factory._name_from_type_and_constructor_name ~type_name ~constructor_name
    in
    assert_equal ~ctxt ~cmp:[%eq: string] ~printer:[%show: string] expected actual
  in
  "_name_from_type_and_constructor_name" >:::
  [ "Handle type t" >:: test ~type_name:"t" ~constructor_name:"A" ~expected:"factory_a"
  ; "Handle other type names" >:: test ~type_name:"u" ~constructor_name:"A" ~expected:"factory_u_a"
  ; "Lowercase ctr name" >:: test ~type_name:"u" ~constructor_name:"RSA" ~expected:"factory_u_rsa"
  ]

let suite =
  "Factory" >:::
  [ test__name_from_type_name
  ; test__name_from_type_and_constructor_name
  ]

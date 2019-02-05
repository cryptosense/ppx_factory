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

let suite =
  "Factory" >:::
  [ test__name_from_type_name
  ]

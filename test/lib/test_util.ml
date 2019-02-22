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

module List_ = struct
  let test_all_ok =
    let test ~input ~expected ctxt =
      let actual = Ppx_factory_lib.Util.List_.all_ok input in
      assert_equal ~ctxt
        ~cmp:[%eq: (int list, int) result]
        ~printer:[%show: (int list, int) result]
        expected
        actual
    in
    "all_ok" >:::
    [ "Empty" >:: test ~input:[] ~expected:(Ok [])
    ; "Ok" >:: test ~input:[Ok 0] ~expected:(Ok [0])
    ; "Error" >:: test ~input:[Error 0] ~expected:(Error 0)
    ; "Longer with error" >:: test ~input:[Ok 0; Ok 1; Error 2; Ok 3] ~expected:(Error 2)
    ; "Longer ok" >:: test ~input:[Ok 0; Ok 1; Ok 2] ~expected:(Ok [0; 1; 2])
    ]

  let suite =
    "List_" >:::
    [ test_all_ok
    ]
end

let suite =
  "Util" >:::
  [ test_suffix_from_type_name
  ; List_.suite
  ]

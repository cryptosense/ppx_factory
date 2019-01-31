let default =
  Ppxlib.Deriving.add
    "default"
    ~str_type_decl:Ppx_factory_lib.Default.from_str_type_decl

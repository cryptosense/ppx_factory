let default =
  Ppxlib.Deriving.add
    "default"
    ~str_type_decl:Ppx_factory_lib.Default.from_str_type_decl
    ~sig_type_decl:Ppx_factory_lib.Default.from_sig_type_decl

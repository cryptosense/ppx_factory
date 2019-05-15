
(library
  (name default_str_no_manifest)
  (modules default_str_no_manifest)
  (preprocess (pps ppx_factory))
)

(rule
  (targets default_str_no_manifest.actual)
  (deps (:pp pp.exe) (:input default_str_no_manifest.ml))
  (action
    (with-stderr-to
      %{targets}
      (bash "./%{pp} -no-color --impl %{input} || true")
    )
  )
)

(alias
  (name runtest)
  (action (diff default_str_no_manifest.expected default_str_no_manifest.actual))
)

(library
  (name default_str_unspecified_type)
  (modules default_str_unspecified_type)
  (preprocess (pps ppx_factory))
)

(rule
  (targets default_str_unspecified_type.actual)
  (deps (:pp pp.exe) (:input default_str_unspecified_type.ml))
  (action
    (with-stderr-to
      %{targets}
      (bash "./%{pp} -no-color --impl %{input} || true")
    )
  )
)

(alias
  (name runtest)
  (action (diff default_str_unspecified_type.expected default_str_unspecified_type.actual))
)

(library
  (name factory_abstract_type)
  (modules factory_abstract_type)
  (preprocess (pps ppx_factory))
)

(rule
  (targets factory_abstract_type.actual)
  (deps (:pp pp.exe) (:input factory_abstract_type.ml))
  (action
    (with-stderr-to
      %{targets}
      (bash "./%{pp} -no-color --impl %{input} || true")
    )
  )
)

(alias
  (name runtest)
  (action (diff factory_abstract_type.expected factory_abstract_type.actual))
)

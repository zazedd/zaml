(lang dune 3.7)

(name zaml)

(generate_opam_files true)

(source
 (github zazed/zaml))

(using menhir 2.1)

(authors "Leonardo Santos")

(maintainers "Leonardo Santos")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name zaml)
 (synopsis "Small OCaml inspired language")
 (description "Small OCaml inspired language")
 (depends ocaml dune menhir ocamllex ppx_expect ppx_inline_test)
 (tags
  (OCaml "functional programming" language types "hindley-milner" "generalized hindley-milner")))

(lang dune 2.9)

(name taparse)

(version 0.1.0)

(generate_opam_files true)

(source
 (github joelburget/typed-algebraic-parsing))

(license BSD-3-Clause)

(authors "Joel Burget")

(maintainers "joelburget@gmail.com")

(package
 (name structures)
 (synopsis "Helpers and laws for algebraic structures")
 ; (description "TODO")
 (depends fmt))

(package
 (name char-class)
 (synopsis "Representation of character classes")
 ; (description "TODO")
 (depends base diet fmt ppx_jane structures))

(package
 (name taparse-regex)
 (synopsis "Ocaml-native regular expressions")
 ; (description "TODO")
 (depends base diet fmt ppx_jane structures char-class))

(package
 (name taparse)
 (synopsis "A Typed, Algebraic Approach to Parsing")
 ; (description "TODO")
 (depends base diet fmt ppx_jane char-class uutf))

(package
 (name ppx_taparse)
 (synopsis "Static code generation for Taparse parsers")
 ; (description "TODO")
 (depends
  base
  ppx_jane
  taparse-regex
  taparse
  (ppxlib
   (>= 0.22.0))))

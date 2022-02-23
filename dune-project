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
 (depends
  (fmt
   (>= 0.9.0))))

(package
 (name char-class)
 (synopsis "Representation of character classes")
 ; (description "TODO")
 (depends
  (base
   (and
    (>= 0.14.0)
    (< v0v.15.0)))
  (diet
   (>= 0.4))
  (fmt
   (>= 0.9.0))
  (ppx_jane
   (and
    (>= v0.14.0)
    (< v0.15.0)))
  structures))

(package
 (name taparse-regex)
 (synopsis "Ocaml-native regular expressions")
 ; (description "TODO")
 (depends
  (base
   (and
    (>= v0.14.0)
    (< v0.15.0)))
  (fmt
   (>= 0.9.0))
  (ppx_jane
   (and
    (>= v0.14.0)
    (< v0.15.0)))
  structures
  char-class))

(package
 (name taparse)
 (synopsis "A Typed, Algebraic Approach to Parsing")
 ; (description "TODO")
 (depends
  (base
   (and
    (>= v0.14.0)
    (< v0.15.0)))
  (fmt
   (>= 0.9.0))
  (ppx_jane
   (and
    (>= v0.14.0)
    (< v0.15.0)))
  char-class
  (uutf
   (>= 1.0.0))))

(package
 (name ppx_taparse)
 (synopsis "Static code generation for Taparse parsers")
 ; (description "TODO")
 (depends
  (base
   (and
    (>= v0.14.0)
    (< v0.15.0)))
  (ppx_jane
   (and
    (>= v0.14.0)
    (< v0.15.0)))
  taparse-regex
  taparse
  (ppxlib
   (>= 0.22.0))))

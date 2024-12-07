(lang dune 3.0)
(name orange)

(generate_opam_files true)

(package
 (name orange)
 (synopsis "")
 (description "")
 (depends
  (ocaml (>= 4.14.0))
  (dune (>= 3.0))
  (core (>= v0.16.0))
  (ppx_jane (>= v0.16.0))
  (ppx_let (>= v0.16.0))
  (ppx_deriving (>= 5.2.1))
  (odoc :with-doc)
  (ounit2 :with-test)))

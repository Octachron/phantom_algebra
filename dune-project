(lang dune 2.5)

(generate_opam_files true)

(name phantom-algebra)
(authors "Florian Angeletti <octa@polychoron.fr>")
(maintainers "Florian Angeletti <octa@polychoron.fr>")
(homepage https://github.com/Octachron/phantom_algebra)
(bug_reports https://github.com/Octachron/phantom_algebra/issues)
(source (github Octachron/phantom_algebra))


(package
  (name phantom-algebra)
  (version 1.0.1)
 (license MIT)
  (synopsis "A strongly-typed tensor library à la GLSL")
  (depends (dune (>= 2.5)) (ocaml (>= 4.06.0)))
  (description "Phantom-algebra is a pure OCaml library implementing strongly-typed
             small tensors with dimensions 0 ≤ 4, rank ≤ 2, and limited to square matrices.
             It makes it possible to manipulate vector and matrix expressions with an
             uniform notation while still catching non-sensical operations at compile time."
)
)

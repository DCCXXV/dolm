(lang dune 3.21)

(name dolm)

(generate_opam_files true)

(source
 (github DCCXXV/dolm))

(authors "DCCXXV")

(maintainers "DCCXXV")

(license MIT)

(package
 (name dolm)
 (synopsis "A Lights Out-style puzzle game")
 (description "Turn every tile dark using the minimum possible steps. 17 levels of increasing difficulty.")
 (depends
  ocaml
  dune
  tsdl
  tsdl-ttf)
 (tags
  (game puzzle "lights out")))

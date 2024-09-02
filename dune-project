(lang dune 1.11)
(name radiocarbon)

(generate_opam_files true)

; Edit these to give your project's URL, authors and maintainers:
(source (github ocurrent/ocurrent-skeleton))
(authors "talex5@gmail.com")
(maintainers "talex5@gmail.com")

(package
 (name radiocarbon)
 (synopsis "An OCurrent pipeline")
 (description "A longer description of the radiocarbon pipeline.")
 (depends
  (ocaml (>= 4.08.0))
  (prometheus-app (>= 1.0))
  digestif
  fmt
  lwt
  cmdliner
  ppx_deriving_yojson)
)

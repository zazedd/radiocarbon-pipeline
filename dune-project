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
 (description "A longer description")
 (depends
  (prometheus-app (>= 1.0))
  (digestif
   fmt
   lwt
   lwt.unix
   cmdliner
   ppx_deriving_yojson.runtime
   prometheus-app.unix
  (ocaml (>= 4.08.0))))

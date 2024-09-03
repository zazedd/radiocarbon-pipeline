(lang dune 3.0)

(name radiocarbon)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)


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

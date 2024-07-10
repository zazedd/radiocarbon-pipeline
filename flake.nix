{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-master.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, nixpkgs-master, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
          };
          new-pkgs = import nixpkgs-master {
            inherit system;
          };
          ourRPackages = pkgs.rPackages.override (old: old // { overrides = {
              sf = pkgs.rPackages.sf.overrideAttrs (attrs: {
                configureFlags = [
                  "--with-proj-lib=${pkgs.lib.getLib pkgs.proj}/lib"
                ];
              });
            };
          });
        in
        with pkgs;
        {
          devShells.default = mkShell {
            buildInputs = [ R git ];
            packages = [ ourRPackages.rcarbon ];
          };

          devShells.dev = mkShell {
            buildInputs = [ ocaml ];
            packages = with nixpkgs; [ 
              dune_3
              opam
              ocamlPackages.bos
              ocamlPackages.astring
              ocamlPackages.bos
              ocamlPackages.cmdliner
              ocamlPackages.duration
              ocamlPackages.logs
              ocamlPackages.fpath
              ocamlPackages.lwt
              ocamlPackages.alcotest
              ocamlPackages.ppx_deriving
              ocamlPackages.prometheus
              ocamlPackages.re
              ocamlPackages.ppx_deriving_yojson
              ocamlPackages.yojson
              ocamlPackages.mirage-crypto
              ocamlPackages.hex
              new-pkgs.ocamlPackages.gitlab-unix
              ocamlPackages.cohttp-lwt-unix
              ocamlPackages.ptime
              ocamlPackages.rresult
              ocamlPackages.capnp
              ocamlPackages.stdint

              capnproto
              sqlite
            ];
          };
        }
      );
}


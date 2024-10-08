{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-master.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };
  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-master,
      nix-filter,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        ourRPackages = pkgs.rPackages.override (
          old:
          old
          // {
            overrides = {
              sf = pkgs.rPackages.sf.overrideAttrs (attrs: {
                configureFlags = [
                  "--with-proj-lib=${pkgs.lib.getLib pkgs.proj}/lib"
                ];
              });
            };
          }
        );
      in
      with pkgs;
      {
        devShells.default = mkShell {
          buildInputs = [
            R
            git
          ];
          packages = [
            ourRPackages.rcarbon
            ourRPackages.ggplot2
            ourRPackages.Hmisc
            ourRPackages.quantreg
          ];
        };

        devShells.dev = mkShell {
          propagatedBuildInputs = [
            pkg-config

            capnproto
            gnuplot
            gmp
            capnproto
            sqlite
            libffi
            libev
            graphviz
            zlib.static
          ];

          packages = [
            nodejs
          ];

          shellHook = ''
            export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:${pkgs.sqlite.out}/lib:${pkgs.libffi.out}/lib;
            export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [ pkgs.zlib.static ]}:$LD_LIBRARY_PATH"
          '';
        };
      }
    );
}

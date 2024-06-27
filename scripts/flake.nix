{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
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
            buildInputs = [ R ];
            packages = [ ourRPackages.rcarbon ];
          };
        }
      );
}


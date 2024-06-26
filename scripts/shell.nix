{ pkgs ? import <nixpkgs> {} }:
  let
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
  pkgs.mkShell {
    buildInputs = [ R ];
    packages = [ ourRPackages.rcarbon ];
  }


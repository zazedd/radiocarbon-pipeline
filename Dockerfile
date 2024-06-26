FROM nixos/nix
COPY scripts/shell.nix shell.nix 
COPY scripts/script.r script.r
COPY input/denmark.csv denmark.csv
RUN nix-shell shell.nix

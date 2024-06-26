(*open Current.Syntax*)
module Git = Current_git
module Docker = Current_docker.Default

let timeout = Duration.of_hour 1
let pull = false

let dockerfile =
  `Contents
    {|
      FROM nixos/nix
      COPY ./scripts/flake.nix flake.nix
      COPY ./scripts/flake.lock flake.lock
      RUN nix develop --extra-experimental-features 'nix-command flakes' 
    |}
  |> Current.return

let v ~repo () =
  Git.Local.repo repo |> Fpath.to_string |> Format.printf "%s@.";
  let src = Git.Local.head_commit repo in
  let image = Docker.build ~dockerfile ~pull ~timeout (`Git src) in
  Docker.run image ~args:[ "Rscript"; "--help" ]

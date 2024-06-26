(*open Current.Syntax*)
module Git = Current_git
module Docker = Current_docker.Default

let timeout = Duration.of_hour 1
let pull = false

let dockerfile =
  `Contents
    {|
      FROM nixos/nix
      COPY scripts/shell.nix shell.nix 
    |}
  |> Current.return

let v ~repo () =
  Git.Local.repo repo |> Fpath.to_string |> Format.printf "%s@.";
  let src = Git.Local.head_commit repo in
  let image = Docker.build ~dockerfile ~pull ~timeout (`Git src) in
  Docker.run image ~run_args:[]
    ~args:[ "nix-shell"; "shell.nix"; "--command"; "Rscript"; "--help" ]

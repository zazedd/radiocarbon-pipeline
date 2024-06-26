(*open Current.Syntax*)
module Git = Current_git
module Docker = Current_docker.Default

let timeout = Duration.of_hour 1
let pull = false

let dockerfile =
  (* The nix-shell command here will cache the result for further use,
     it is not for actually spawning a shell *)
  `Contents
    {|
      FROM nixos/nix
      COPY scripts/shell.nix shell.nix 
      COPY scripts/script.r script.r
      COPY inputs/denmark.csv denmark.csv
      RUN nix-shell shell.nix
    |}
  |> Current.return

let v ~repo () =
  let src = Git.Local.head_commit repo in
  let image = Docker.build ~dockerfile ~pull ~timeout (`Git src) in
  Docker.run image ~run_args:[]
    ~args:
      [ "nix-shell"; "shell.nix"; "--command"; "Rscript script.r denmark.csv" ]

(*
   TODO: 1
   The inputs are fixed, we should watch over the repository, specifically the inputs/ directory
   and check if there are new/modified files. If so, we should run them through our script.

   TODO: 2
   The outputs currently stay inside the docker container, we should remove and place them inside
   the outputs/ directory at some point.

   TODO: 3
   Currently only a local repository is considered, we should consider remote repositories as well.

   TODO: 4
   If the script produces a PDF output, display it using Github's renderer.

   FIXME: 1
   Nix envirnoment using flakes instead of nix-shell, as it doesnt pin the package versions and is not
   reproducible.
*)

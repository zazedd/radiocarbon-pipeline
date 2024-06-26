(*open Current.Syntax*)
module Git = Current_git
module Nix = Current_nix.Default

let timeout = Duration.of_hour 1
let pull = false

let v ~repo () =
  let src = Git.Local.head_commit repo in
  Nix.build ~timeout (`Git src)

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

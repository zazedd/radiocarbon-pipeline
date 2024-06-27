module Git = Current_git
module Nix = Current_nix.Default

let timeout = Duration.of_hour 1
let pull = false
let last_commit : Git.Commit.t option ref = ref None

let v ~repo () =
  let src = Git.Local.head_commit repo in
  (*let+ _ = src in*)
  Nix.shell
    ~args:
      [
        [
          "Rscript"; "scripts/script.r"; "inputs/denmark.csv"; "outputs/out.csv";
        ];
        [ "echo"; "hello" ];
      ]
    ~timeout (`Git src)

(*
   TODO: 1
   The inputs are fixed, we should watch over the repository, specifically the inputs/ directory
   and check if there are new/modified files. If so, we should run them through our script.

   TODO: 2
   The outputs currently stay inside the temporary folder, we should remove and place them inside
   the outputs/ directory at some point, commit them and push them to the repo.

   TODO: 3
   Currently only a local repository is considered, we should consider remote repositories as well.

   TODO: 4
   If the script produces a PDF output, display it using Github's renderer.
*)

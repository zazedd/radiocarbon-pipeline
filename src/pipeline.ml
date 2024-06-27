(*open Current.Syntax*)
(*open Lwt.Infix*)
module Git = Current_git
module Nix = Current_nix.Default

let timeout = Duration.of_hour 1
let pull = false
let last_commit : Git.Commit.t option ref = ref None

(*let handle_repo_change src commit =*)
(*  let config = Current.Config.now |> Current_incr.observe |> Option.get in*)
(*  let job =*)
(*    Current.Job.create*)
(*      ~switch:(Current.Switch.create ~label:"test" ())*)
(*      ~label:"Test" ~config ()*)
(*  in*)
(*  Current.Job.start ~timeout job ~level:Current.Level.Harmless >>= fun () ->*)
(*  Git.with_checkout ~job commit @@ fun dir ->*)
(*  last_commit := Some commit;*)
(*  Nix.shell*)
(*    ~args:*)
(*      [*)
(*        [*)
(*          "Rscript"; "scripts/script.r"; "inputs/denmark.csv"; "outputs/out.csv";*)
(*        ];*)
(*        [ "echo"; "hello" ];*)
(*      ]*)
(*    ~timeout (`Git src)*)
(*  >|= fun res -> res*)

let v ~repo () =
  let src = Git.Local.head_commit repo in
  (*let+ commit = src in*)
  (*handle_repo_change src commit*)
  Nix.shell ~args:[ [ "dune"; "runtest" ] ] ~timeout (`Git src)

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

   FIXME: 1
   Nix envirnoment using flakes instead of nix-shell, as it doesnt pin the package versions and is not
   reproducible.
*)

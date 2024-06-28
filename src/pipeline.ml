open Current.Syntax
module Git = Current_git
module Nix = Current_nix.Default

let pp_sp_label = Fmt.(option (sp ++ string))
let timeout = Duration.of_hour 1
let pull = false
let last_commit : Git.Commit.t Current.t option ref = ref None

module Git_cache = Current_cache.Make (Cache)

let v ~repo () =
  let src = Git.Local.head_commit repo in
  let last = match !last_commit with None -> src | Some c -> c in
  let+ last_dir = Current_gitfile.directory_contents last (Fpath.v "src")
  and+ current_dir = Current_gitfile.directory_contents src (Fpath.v "src") in
  Format.printf "some@.";
  List.iter2
    (fun (last_path, last_a) (current_path, current_a) ->
      Format.printf "%s\nand %s:\n%s\nvs %s@."
        (Fpath.to_string last_path)
        (Fpath.to_string current_path)
        current_a last_a)
    last_dir current_dir;
  last_commit := Some src

(*  Nix.shell*)
(*    ~args:*)
(*      [*)
(*        [*)
(*          "Rscript";*)
(*          "scripts/script.r";*)
(*          "inputs/denmark.csv";*)
(*          "outputs/out.csv";*)
(*        ];*)
(*        [ "echo"; "hello" ];*)
(*      ]*)
(*    ~timeout (`Git src)*)
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

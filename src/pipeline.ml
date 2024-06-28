open Current.Syntax
module Git = Current_git
module Nix = Current_nix.Default

let pp_sp_label = Fmt.(option (sp ++ string))
let timeout = Duration.of_hour 1
let pull = false

let v ~repo () =
  let src = Git.Local.head_commit repo in
  let commit_path = Fpath.v ".commit" in
  Current.component "grab previous commit%a" pp_sp_label None
  |>
  let** current_commit = src in
  let last =
    Bos.OS.File.read commit_path
    |> Result.value ~default:(Git.Commit.marshal current_commit)
    |> Git.Commit.unmarshal |> Current.return
  in
  let+ last_dir = Current_gitfile.directory_contents last (Fpath.v "inputs")
  and+ current_dir =
    Current_gitfile.directory_contents src (Fpath.v "inputs")
  in
  let file_name f = Fpath.rem_prefix (Fpath.parent f) f |> Option.get in
  let considered =
    List.fold_left
      (fun acc (path1, hash1) ->
        let file =
          List.find_opt
            (fun (path2, _) -> file_name path1 = file_name path2)
            last_dir
        in
        match file with
        | None -> path1 :: acc
        | Some (_, hash2) -> if hash1 <> hash2 then path1 :: acc else acc)
      [] current_dir
  in
  List.iter (fun a -> a |> Fpath.to_string |> Format.printf "%s@.") considered;
  Bos.OS.File.write commit_path (Git.Commit.marshal current_commit)
  |> Result.value ~default:()

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

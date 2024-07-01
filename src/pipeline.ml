open Current.Syntax
module Git = Current_git
module Nix = Current_nix.Default

let pp_sp_label = Fmt.(option (sp ++ string))
let timeout = Duration.of_hour 1
let pull = false
let file_name f = Fpath.rem_prefix (Fpath.parent f) f |> Option.get

let new_and_changed_files curr last =
  List.fold_left
    (fun acc (path1, hash1) ->
      let file =
        List.find_opt (fun (path2, _) -> file_name path1 = file_name path2) last
      in
      match file with
      | None -> path1 :: acc
      | Some (_, hash2) -> if hash1 <> hash2 then path1 :: acc else acc)
    [] curr
  |> List.map (fun a ->
         let name = Fpath.base a in
         let folder = Fpath.v "inputs" in
         Fpath.(folder // name) |> Fpath.to_string)

let generate_script_args c =
  List.map
    (fun file ->
      let output_folder = Fpath.v "outputs/" in
      let output_file =
        file |> Fpath.v |> Fpath.base |> Fpath.rem_ext |> Fpath.add_ext "out"
        |> Fpath.add_ext "csv"
      in
      [
        "Rscript";
        "scripts/script.r";
        file;
        Fpath.(output_folder // output_file) |> Fpath.to_string;
      ])
    c

let v ~repo () =
  let src = Git.Local.head_commit repo in
  Unix.sleep 5;
  let commit_path = Fpath.v ".commit" in
  Current.component "grab previous commit%a" pp_sp_label None
  |>
  let** current_commit = src in
  let last =
    Bos.OS.File.read commit_path
    |> Result.value ~default:(Git.Commit.marshal current_commit)
    |> Git.Commit.unmarshal |> Current.return
  in
  Current.component "grab new/changed files%a" pp_sp_label None
  |>
  let** last_dir =
    Current_gitfile.directory_contents_hashes last (Fpath.v "inputs")
      ~label:"previous"
  and* current_dir =
    Current_gitfile.directory_contents_hashes src (Fpath.v "inputs")
      ~label:"current"
  in
  let script_runs =
    new_and_changed_files current_dir last_dir |> generate_script_args
  in
  Bos.OS.File.write commit_path (Git.Commit.marshal current_commit)
  |> Result.value ~default:();
  if List.length script_runs = 0 then () |> Current.return
  else
    let* _ =
      Nix.shell ~args:script_runs ~timeout (`Git src) ~label:"R-script"
    in
    let* x =
      Current_gitfile.commit ~label:"new outputs" [ "--all"; "-m"; "test" ]
    in
    x |> Current.return

(*
   TODO: 1
   The inputs are fixed, we should watch over the repository, specifically the inputs/ directory
   and check if there are new/modified files. If so, we should run them through our script.
   DONE!

   TODO: 2
   The outputs currently stay inside the temporary folder, we should remove and place them inside
   the outputs/ directory at some point
   DONE!

   TODO: 3
   Commit them and push them to the repo.

   TODO: 4
   Currently only a local repository is considered, we should consider remote repositories as well.

   TODO: 5
   If the script produces a PDF output, display it using Github's renderer.
*)

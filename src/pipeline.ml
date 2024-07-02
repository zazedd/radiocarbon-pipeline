open Current.Syntax
module Git = Current_git
module Github = Current_github
module Nix = Current_nix.Default

let pp_sp_label = Fmt.(option (sp ++ string))
let timeout = Duration.of_hour 1
let pull = false
let output_folder = Fpath.v "outputs/"
let file_name f = Fpath.rem_prefix (Fpath.parent f) f |> Option.get

let output_file_name f =
  let output_file =
    f |> Fpath.v |> Fpath.base |> Fpath.rem_ext |> Fpath.add_ext "out"
    |> Fpath.add_ext "csv"
  in
  Fpath.(output_folder // output_file) |> Fpath.to_string

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
      [
        "Rscript";
        "scripts/script.r";
        file;
        output_file_name file;
        "Site";
        "Aussois";
      ])
    c

(* let v ~repo () = *)
(*   let src = Git.Local.head_commit repo in *)
(*   let commit_path = Fpath.v ".commit" in *)
(*   Current.component "grab previous commit%a" pp_sp_label None *)
(*   |> *)
(*   let** current_commit = src in *)
(*   let last = *)
(*     Bos.OS.File.read commit_path *)
(*     |> Result.value ~default:(Git.Commit.marshal current_commit) *)
(*     |> Git.Commit.unmarshal |> Current.return *)
(*   in *)
(*   Current.component "grab new/changed files%a" pp_sp_label None *)
(*   |> *)
(*   let** last_dir = *)
(*     Current_gitfile.directory_contents_hashes last (Fpath.v "inputs") *)
(*       ~label:"previous" *)
(*   and* current_dir = *)
(*     Current_gitfile.directory_contents_hashes src (Fpath.v "inputs") *)
(*       ~label:"current" *)
(*   in *)
(*   let files = new_and_changed_files current_dir last_dir in *)
(*   let script_runs = generate_script_args files in *)
(*   let output_files = List.map output_file_name files in *)
(*   Bos.OS.File.write commit_path (Git.Commit.marshal current_commit) *)
(*   |> Result.value ~default:(); *)
(*   if List.length script_runs = 0 then () |> Current.return *)
(*   else *)
(*     let* _ = *)
(*       Nix.shell ~args:script_runs ~timeout (`Git src) ~label:"R-script" *)
(*     in *)
(*     let* _ = *)
(*       Current_gitfile.add ~label:"new outputs" (".commit" :: output_files) *)
(*     in *)
(*     Current_gitfile.commit_push ~label:"new outputs" [ "--all"; "-m"; "test" ] *)

let v ~repo () =
  let src = Git.Local.head_commit repo in
  let file = Bos.OS.File.read (Fpath.v "inputs/denmark.csv") |> Result.get_ok in
  Format.printf "file: %s@." file;
  let new_hash = Digestif.SHA512.digest_string file |> Digestif.SHA512.to_hex in
  let hash = new_hash |> Current.return in
  let+ Current_gitfile.Raw.Test.Value.{ digest } =
    Current_gitfile.grab_hash hash "inputs/denmark.csv"
  and+ _ = src in
  Format.printf "old: %s@." digest;
  Format.printf "new: %s@." new_hash

(* let script_runs = generate_script_args files in *)
(* if List.length script_runs = 0 then () |> Current.return *)
(* else *)
(*   let* _ = *)
(*     Nix.shell ~args:script_runs ~timeout (`Git src) ~label:"R-script" *)
(*   in *)
(*   let* _ = *)
(*     Current_gitfile.add ~label:"new outputs" (".commit" :: output_files) *)
(*   in *)
(*   Current_gitfile.commit_push ~label:"new outputs" [ "--all"; "-m"; "test" ] *)

(*
   TODO: 1
   The inputs are fixed, we should watch over the repository, specifically the inputs/ directory
   and check if there are new/modified files. If so, we should run them through our script.
   DONE!

   HACK: 
   Marshaling the last commit to a file. We should try to use the cache:
   Key: filename
   Value: hash of the file
   While getting, if the filehash isnt there it will get built. If the hash of the file does not match the
   new one we should invalidate that entry and create a new one.

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

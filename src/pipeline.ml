open Current.Syntax
module Git = Current_git
module Github = Current_github
module Nix = Current_nix.Default

let pp_sp_label = Fmt.(option (sp ++ string))
let timeout = Duration.of_hour 1
let pull = false
let output_folder = Fpath.v "outputs/"
let file_name f = Fpath.rem_prefix (Fpath.parent f) f |> Option.get

(* let output_file_name f = *)
(*   let output_file = *)
(*     f |> Fpath.v |> Fpath.base |> Fpath.rem_ext |> Fpath.add_ext "out" *)
(*     |> Fpath.add_ext "csv" *)
(*   in *)
(*   Fpath.(output_folder // output_file) |> Fpath.to_string *)

(* let new_and_changed_files curr last = *)
(*   List.fold_left *)
(*     (fun acc (path1, hash1) -> *)
(*       let file = *)
(*         List.find_opt (fun (path2, _) -> file_name path1 = file_name path2) last *)
(*       in *)
(*       match file with *)
(*       | None -> path1 :: acc *)
(*       | Some (_, hash2) -> if hash1 <> hash2 then path1 :: acc else acc) *)
(*     [] curr *)
(*   |> List.map (fun a -> *)
(*          let name = Fpath.base a in *)
(*          let folder = Fpath.v "inputs" in *)
(*          Fpath.(folder // name) |> Fpath.to_string) *)

(* let generate_script_args c = *)
(*   List.map *)
(*     (fun file -> *)
(*       [ *)
(*         "Rscript"; *)
(*         "scripts/script.r"; *)
(*         file; *)
(*         output_file_name file; *)
(*         "Site"; *)
(*         "Aussois"; *)
(*       ]) *)
(*     c *)

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

let new_and_changed_files src inputs =
  List.map
    (fun file ->
      let content = Bos.OS.File.read file |> Result.get_ok in
      let digest =
        content |> Digestif.SHA512.digest_string |> Digestif.SHA512.to_hex
      in
      let k = Current_gitfile.Raw.Test.Value.{ digest } |> Current.return in
      Current_gitfile.grab_hash src k (file |> Fpath.to_string))
    inputs

let v ~repo () =
  let src = Git.Local.head_commit repo in
  let in_path = Fpath.v "inputs/" in
  let* _ = src in
  let inputs = Bos.OS.Dir.contents in_path |> Result.get_ok in
  let+ n_c_files = inputs |> new_and_changed_files src |> Current.list_seq in
  List.iter
    (fun f ->
      let x = match f with Some s -> "file:" ^ s | None -> "none" in
      Format.printf "%s@." x)
    n_c_files

(* let* contents = contents in *)
(* let* new_hash = *)
(*   Digestif.SHA512.digest_string contents *)
(*   |> Digestif.SHA512.to_hex |> Current.return *)
(* in *)
(* let new_hash_value = *)
(*   Current_gitfile.Raw.Test.Value.{ digest = new_hash } |> Current.return *)
(* in *)
(* let+ new_or_changed_file = *)
(*   Current_gitfile.grab_hash src new_hash_value "inputs/test" *)
(* in *)
(* Format.printf "file: %s@." contents; *)
(* match new_or_changed_file with *)
(* | Some file -> Format.printf "CHANGED! %s@." file *)
(* | None -> () *)

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

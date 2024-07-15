open! Import
open Current.Syntax
module CNix = Custom_nix
module CGit = Custom_git
module PCache = Pipeline_cache

type t = [ `No_inputs | `Deleted_inputs | `New_inputs ]

let timeout = Duration.of_hour 1

let status_of_state result =
  let main_status =
    match result with
    | Ok (status : t) ->
        let msg =
          match status with
          | `No_inputs -> "Finished. No inputs have changed"
          | `Deleted_inputs -> "Finished. Some inputs were deleted."
          | `New_inputs ->
              "Finished. Check the output folder for new/changed outputs."
        in
        Github.Api.CheckRunStatus.v ?text:(Some msg) (`Completed `Success)
    | Error (`Active _) ->
        Github.Api.CheckRunStatus.v ?text:(Some "Running...") `InProgress
    | Error (`Msg m) ->
        Github.Api.CheckRunStatus.v ?text:(Some m) (`Completed (`Failure m))
  in
  main_status |> Current.return

let fetch_commit ~github ~repo () =
  let head = Github.Api.head_commit github repo in
  let commit_id = Current.map Github.Api.Commit.id head in
  (head, Git.fetch commit_id)

let file_script_output_config_outputfolder ((csv, _), cfg) =
  let csv_folder = csv |> Fpath.split_base |> fst in
  let root = csv_folder |> Fpath.parent in
  let inputs_folder = Fpath.base csv_folder in
  let is_on_inputs = Fpath.v "inputs/" = inputs_folder in
  let config =
    match cfg with None -> Fpath.(root / "config") | Some (c, _) -> c
  in
  let config_contents = Bos.OS.File.read_lines config |> Result.get_ok in
  let+ script =
    Config_cache.grab_script ~path:config ~contents:config_contents
  in
  let script_no_ext = Fpath.v script |> Fpath.rem_ext |> Fpath.to_string in
  let output_file =
    csv |> Fpath.base |> Fpath.rem_ext
    |> Fpath.add_ext script_no_ext
    |> Fpath.add_ext "csv"
  in
  let output_folder =
    if is_on_inputs then Fpath.(root / "outputs")
    else Fpath.((root |> Fpath.parent) / "outputs" // inputs_folder)
  in
  ( csv,
    script,
    Fpath.(output_folder // output_file) |> Fpath.to_string,
    config,
    output_folder )

let generate_script_args f_cfgs =
  List.map
    (fun (file, script, output_file, config, output_folder) ->
      Bos.OS.Dir.create output_folder |> Result.get_ok |> ignore;
      [
        "Rscript";
        "scripts/" ^ script;
        file |> Fpath.to_string;
        output_file;
        config |> Fpath.to_string;
      ])
    f_cfgs

let rec new_hashes in_path =
  let inputs = Bos.OS.Dir.contents in_path |> Result.get_ok in
  List.fold_left
    (fun acc inp ->
      if Bos.OS.Dir.exists inp |> Result.get_ok then new_hashes inp @ acc
      else
        (let content = Bos.OS.File.read inp |> Result.get_ok in
         ( inp,
           content |> Digestif.SHA512.digest_string |> Digestif.SHA512.to_hex ))
        :: acc)
    [] inputs

let run script_runs local_src github_commit repo_path output_files () =
  CNix.shell ~args:script_runs ~timeout (`Git local_src) ~label:"R-script"
  |> Pipeline_cache.run ~path:repo_path ~output_files ~github_commit
       ~nix_args:script_runs
       ~remote_origin:"git@github.com:zazedd/inputs-outputs-R14C.git"
       ~commit_message:"OCurrent: Automatic Push" ~label:"Git commands"

let vv ~src ~local_src ~github_commit () =
  let* commit = src and* github_commit = github_commit in
  let repo_path = Git.Commit.repo commit in
  let in_path = Fpath.(repo_path / "inputs") in
  let file_hashes = new_hashes in_path |> Current.return in
  let* n_c_files = CGit.grab_new_and_changed file_hashes in_path in
  match n_c_files with
  | Some l ->
      Logs.info (fun f -> f "Some inputs have changed.");
      let* f_o_cfg_of =
        List.map file_script_output_config_outputfolder l |> Current.list_seq
      in
      let script_runs = f_o_cfg_of |> generate_script_args in
      let output_files =
        List.map (fun (_, _, _, _, out_folder) -> out_folder) f_o_cfg_of
      in
      if List.length script_runs = 0 then `Deleted_inputs |> Current.return
      else
        let+ _ =
          run script_runs local_src github_commit repo_path output_files ()
        in
        `New_inputs
  | _ ->
      let msg = "No inputs have changed." in
      Logs.info (fun f -> f "%s" msg);
      `No_inputs |> Current.return

let v ~local ~installation () =
  let local_src = Git.Local.head_commit local in
  let github = Current.map Github.Installation.api installation in
  Github.Installation.repositories installation
  |> Current.list_iter ~collapse_key:"repos" (module Github.Api.Repo)
     @@ fun repo ->
     let* repo = Current.map Github.Api.Repo.id repo and* github = github in
     let github_commit, src = fetch_commit ~github ~repo () in
     let r = vv ~src ~local_src ~github_commit () in
     Current.component "pipeline"
     |>
     let** status = Current.state r in
     let+ _ =
       status |> status_of_state
       |> Github.Api.CheckRun.set_status github_commit "Pipeline Execute"
     in
     ()

(*
   NOTE: Project is split into 2 repos
   1 -> for the pipeline, nix shell and scripts, possibly these last two will move
   2 -> for the inputs and outputs of the pipeline

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
   DONE!

   TODO: 4
   Currently only a local repository is considered, we should consider remote repositories as well.
   DONE!

   TODO: 5
   Figure out a way to pass more information to the script -> individual config files for all 
   DONE!

   TODO: 6
   Add status to the git commits, like a GitHub action
   DONE!

   TODO: 7
   If outputs dont change, update status with a checkmark that says nothing changed
   DONE!

   TODO: 8
   If the script produces a PDF output, also push it over

   TODO: 9
   Add other branches, not only main

   TODO: 
   Create another v () that matches on the current state and sends the github status
   DONE!

   TODO: 
   individual configs; DONE!
   choose which script to run; DONE!
   default config; DONE!
   queued -> in progress
   add more columns to the script, median value, weighted mean, max and min
   PDF files

   TODO: 
   mention christophers username in the commit message body
*)

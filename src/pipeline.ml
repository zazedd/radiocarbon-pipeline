open! Import
open Current.Syntax
module CNix = Custom_nix
module CGit = Custom_git

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
        Github.Api.CheckRunStatus.v ?text:(Some "Running...") `Queued
    | Error (`Msg m) ->
        Github.Api.CheckRunStatus.v ?text:(Some m) (`Completed (`Failure m))
  in
  main_status |> Current.return

let fetch_commit ~github ~repo () =
  let head = Github.Api.head_commit github repo in
  let commit_id = Current.map Github.Api.Commit.id head in
  (head, Git.fetch commit_id)

let output_and_config_file_names f =
  let f = f |> Fpath.v in
  let path = f |> Fpath.split_base |> fst |> Fpath.split_base |> fst in
  let output_folder = Fpath.(path / "outputs") in
  let output_file =
    f |> Fpath.base |> Fpath.rem_ext |> Fpath.add_ext "out"
    |> Fpath.add_ext "csv"
  in
  ( Fpath.(path / "config") |> Fpath.to_string,
    Fpath.(output_folder // output_file) |> Fpath.to_string )

let generate_script_args c =
  List.map
    (fun file ->
      let config_file, output_file = output_and_config_file_names file in
      [ "Rscript"; "scripts/script.r"; file; output_file; config_file ])
    c

let new_hashes in_path =
  let inputs = Bos.OS.Dir.contents in_path |> Result.get_ok in
  List.map
    (fun file ->
      let content = Bos.OS.File.read file |> Result.get_ok in
      (file, content |> Digestif.SHA512.digest_string |> Digestif.SHA512.to_hex))
    inputs

let run script_runs src local_src github_commit repo_path in_path output_files v
    () =
  (* let test = List.fold_left (fun acc s -> acc ^ ", " ^ s) "" output_files in *)
  (* let commit_msg = Format.sprintf "'OCurrent: Automatic push %s'" test in *)
  CNix.shell ~args:script_runs ~timeout (`Git local_src) ~label:"R-script"
  |> CGit.add ~label:"new outputs" ~path:repo_path ~github_commit output_files
  |> CGit.rm_origin ~label:"https" ~path:repo_path ~github_commit
  |> CGit.add_origin ~label:"ssh" ~path:repo_path ~github_commit
       "git@github.com:zazedd/inputs-outputs-R14C.git"
  |> CGit.commit_push ~label:"new outputs" ~path:repo_path ~github_commit
       [ "--all"; "-m"; "'OCurrent: Automatic push'" ]
  |> CGit.grab_hashes src ~test:true v in_path
(* let _ = *)
(*   set_commit_status github_commit "Finished" (`Completed `Success) *)
(*     "Pipeline execute" *)
(* in *)

let vv ~src ~local_src ~github_commit () =
  let* commit = src in
  let repo_path = Git.Commit.repo commit in
  let in_path = Fpath.(repo_path / "inputs") in
  let file_hashes = new_hashes in_path in
  let v = CGit.Raw.Git_hash.Value.{ files = file_hashes } |> Current.return in
  let* n_c_files = CGit.grab_hashes src ~test:false v in_path src in
  match n_c_files with
  | Some l ->
      Logs.info (fun f -> f "Some inputs have changed.");
      let files = List.map (fun f -> fst f |> Fpath.to_string) l in
      let script_runs = files |> generate_script_args in
      let output_files =
        List.map (fun a -> output_and_config_file_names a |> snd) files
      in
      if List.length script_runs = 0 then `Deleted_inputs |> Current.return
      else
        let+ _ =
          run script_runs src local_src github_commit repo_path in_path
            output_files v ()
        in
        `New_inputs
  | None ->
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

   ISSUE: If the repo has just been set up, no files with run through the script, probably

   ISSUE: If we delete a file, the status says its set to two different values on the same step

   ISSUE: Some git status are being cached and used where they shouldnt sometimes

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
   Figure out a way to pass more information to the script
   KINDA DONE!

   TODO: 6
   Add status to the git commits, like a GitHub action
   WORKING ON IT

   TODO: 7
   If outputs dont change, update status with a checkmark that says nothing changed

   TODO: 8
   If the script produces a PDF output, also push it over

   TODO: 9
   Add other branches, not only main

   TODO: 
   Create another v () that matches on the current state and sends the github status 

   Ideas for script args:
   1  NOTE: Inside the nix shell we can define environment variables. WE can call with script with them, 
            it is just a matter of changing the value of each if we want something different:
            -> Rscript scripts/script.r inputs/input.csv outputs/outputs.csv $SCRIPT_COLUMN $SCRIPT_VALUE $SCRIPT_STEP $SCRIPT_SIGMA

      ISSUE: This presents an issue, which are named args. Hard things to deal with. Also we should not have anyone mess with the nix files.

  
  2.  NOTE: A file containing a simple definition of values, that gets read by the script and automatically used. Stored in the io repo
            -> ```file
            step=5
            sigma=0.95
            column=Site
            value=Aussois
            ```
            -> ```file
            step=20
            sigma=0.90
            column=
            value=
            ```

       ISSUE: A small issue: this file needs to be changed everytime we want to add/modify something, before adding/modifying it.
              That could be annoying
*)

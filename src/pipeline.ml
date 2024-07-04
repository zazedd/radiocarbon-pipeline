open Current.Syntax
module Git = Current_git
module Github = Current_github
module CNix = Custom_nix
module CGit = Custom_git

let pp_sp_label = Fmt.(option (sp ++ string))
let timeout = Duration.of_hour 1
let pull = false

let fetch_commit ~github ~repo () =
  let head = Github.Api.head_commit github repo in
  let commit_id = Current.map Github.Api.Commit.id head in
  (head, Git.fetch commit_id)

let output_file_name f =
  let f = f |> Fpath.v in
  let output_folder =
    let p = f |> Fpath.split_base |> fst |> Fpath.split_base |> fst in
    Fpath.(p / "outputs")
  in
  let output_file =
    f |> Fpath.base |> Fpath.rem_ext |> Fpath.add_ext "out"
    |> Fpath.add_ext "csv"
  in
  Fpath.(output_folder // output_file) |> Fpath.to_string

let generate_script_args c =
  List.map
    (fun file -> [ "Rscript"; "scripts/script.r"; file; output_file_name file ])
    c

let new_hashes in_path =
  let inputs = Bos.OS.Dir.contents in_path |> Result.get_ok in
  List.map
    (fun file ->
      let content = Bos.OS.File.read file |> Result.get_ok in
      (file, content |> Digestif.SHA512.digest_string |> Digestif.SHA512.to_hex))
    inputs

let v ~local ~installation () =
  let local_src = Git.Local.head_commit local in
  let github = Current.map Github.Installation.api installation in
  Github.Installation.repositories installation
  |> Current.list_iter ~collapse_key:"test" (module Github.Api.Repo)
     @@ fun repo ->
     let* repo = Current.map Github.Api.Repo.id repo and* github = github in
     let github_commit, src = fetch_commit ~github ~repo () in
     let* commit = src in
     let repo_path = Git.Commit.repo commit in
     let in_path = Fpath.(repo_path / "inputs") in
     let file_hashes = new_hashes in_path in
     let v =
       CGit.Raw.Git_hash.Value.{ files = file_hashes } |> Current.return
     in
     (* let status = *)
     (*   Github.Api.CheckRunStatus.v ?text:(Some "Running...") `InProgress *)
     (*   |> Current.return *)
     (* in *)
     (* let _ = *)
     (*   Github.Api.CheckRun.set_status github_commit "Pipeline execute" status *)
     (* in *)
     let* n_c_files = CGit.grab_hashes src v in_path in
     match n_c_files with
     | Some l ->
         Logs.info (fun f -> f "Some inputs have changed.");
         let files = List.map (fun f -> fst f |> Fpath.to_string) l in
         let script_runs = files |> generate_script_args in
         let output_files = List.map output_file_name files in
         if List.length script_runs = 0 then () |> Current.return
         else
           let* _ =
             CNix.shell ~args:script_runs ~timeout (`Git local_src)
               ~label:"R-script"
           in
           let* _ =
             CGit.add ~label:"new outputs" ~path:repo_path output_files
           in
           let* _ = CGit.rm_origin ~label:"https" ~path:repo_path () in
           let* _ =
             CGit.add_origin ~label:"ssh" ~path:repo_path
               "git@github.com:zazedd/inputs-outputs-R14C.git"
           in
           let* _ =
             CGit.commit_push ~label:"new outputs" ~path:repo_path
               [ "--all"; "-m"; "'OCurrent: Automatic push'" ]
           in
           let* _ = CGit.grab_hashes src v in_path in
           let status =
             Github.Api.CheckRunStatus.v ?text:(Some "Finished.")
               (`Completed `Success)
             |> Current.return
           in
           let _ =
             Github.Api.CheckRun.set_status github_commit "Pipeline execute"
               status
           in
           () |> Current.return
     | _ -> Logs.info (fun f -> f "No inputs have changed.") |> Current.return

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
   If the script produces a PDF output, also push it over

   TODO: 6
   Figure out a way to pass more information to the script

   TODO: 7
   Add status to the git commits, like a GitHub action

   Ideas for script args:
   1  NOTE: Inside the nix shell we can define environment variables. WE can call with script with them, 
            it is just a matter of changing the value of each if we want something different:
            -> Rscript scripts/script.r inputs/input.csv outputs/outputs.csv $SCRIPT_COLUMN $SCRIPT_VALUE $SCRIPT_STEP $SCRIPT_SIGMA

      ISSUE: This presents an issue, which are named args. Hard things to deal with. Also we should not have anyone mess with the nix files.

  
  2.  NOTE: A file containing a simple definition of values, that gets read by the script and automatically used. Stored in the io repo
            -> ```file
            column=Site
            value=Aussois
            step=5
            sigma=0.95
            ```
            -> ```file
            column=
            value=
            step=20
            sigma=0.90
            ```

       ISSUE: A small issue: this file needs to be changed everytime we want to add/modify something, before adding/modifying it.
              That could be annoying
*)

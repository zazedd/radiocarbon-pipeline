open! Import
open Current.Syntax
module GCache = Git_cache
module JCache = Job_cache
module FCache = Folder_cache
module CCache = Config_cache
module HCache = Hash_cache

(* type t = [ `Nothing_changed | `Changed_inputs ] *)

let timeout = Duration.of_hour 1

let status_of_state result =
  let main_status =
    match result with
    | Ok status ->
        let msg =
          match status with
          | `No_changes -> "Nothing has changed."
          | `Csv_changes ->
              "One or more CSV files have been changed/added. Check the \
               outputs folder for modified/new files."
          | `Config_changes ->
              "One or more configuration files have been changed. Check the \
               outputs folder for modified/new files."
          | `Script_changes ->
              "One or more script has been changed. Check the outputs folder \
               for modified/new files."
          | `Multiple_changes ->
              "There have been multiple changes. Check the outputs folder for \
               modified/new files."
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

let generate_new_jobs ~script_files src fc =
  let+ { script; input; config } =
    CCache.grab_script ~script_files ~fc src |> HCache.get_hashes ~src ~fc
  in
  JCache.{ script; input; config }

let vv ~src ~local_src ~github_commit () : Import.status Current.t =
  let inputs = "inputs"
  and outputs = "outputs"
  and scripts = "scripts"
  and default_config = "config" in
  let* _ = github_commit
  and* input_files =
    FCache.read_input_folder ~where:inputs ~config:default_config src
  and* script_files = FCache.read_folder ~label:scripts ~where:scripts src in
  let input_files = input_files.files |> Current.return in
  let script_files = script_files.files in
  let+ _ =
    Current.list_map ~collapse_key:"jobs"
      (module FCache)
      (fun fc ->
        generate_new_jobs ~script_files src fc
        |> JCache.run_job ~local_src ~src ~inputs ~outputs)
      input_files
    |> GCache.add_commit_push ~label:"outputs"
         ~remote_origin:"git@github.com:zazedd/inputs-outputs-R14C.git"
         ~commit_message:"OCurrent: Automatic push" src
  and+ s = GCache.diff ~label:"check for changes" src in
  s

let v ~local ~installation () =
  let local_src = Git.Local.head_commit local in
  let github = Current.map Github.Installation.api installation in
  Github.Installation.repositories installation
  |> Current.list_iter ~collapse_key:"repos" (module Github.Api.Repo)
     @@ fun repo ->
     let* repo = Current.map Github.Api.Repo.id repo and* github = github in
     let github_commit, src = fetch_commit ~github ~repo () in
     let r = vv ~src ~local_src ~github_commit () in
     Current.component "finished"
     |>
     let** status = Current.state r in
     status |> status_of_state
     |> Github.Api.CheckRun.set_status github_commit "Pipeline Execute"

(*
   NOTE: Project is split into 2 repos
   1 -> for the pipeline and nix shell
   2 -> for the inputs outputs, and scripts of the pipeline

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
   Create another v () that matches on the current state and sends the github status
   DONE!

   TODO: 9
   individual configs; DONE!

   TODO: 10
   choose which script to run; DONE!

   TODO: 11
   default config; DONE!

   WEEK 4

   TODO: 12
   queued -> in progress; DONE!
   PR THIS TO OCURRENT

   TODO: 13
   Support for nested folders; DONE!

   TODO: 14
   add more columns to the script, median value, weighted mean, max and min

   TODO: 15
   PDF files; DONE!
   Change their name to time stamps

   TODO: 16
   Whenever a script is changed, recompute the files dependant on it

   TODO: 17
   Christopher email setup; DONE!

   TODO: 18
   PR's

   CKDE -> ggplot2
   weighted mean on rows and on the cumulative row
*)

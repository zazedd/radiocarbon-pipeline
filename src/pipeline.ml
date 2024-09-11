open! Import
open Current.Syntax
module GCache = Git_cache
module JCache = Job_cache
module FCache = Folder_cache
module SCache = Script_cache
module HCache = Hash_cache

let timeout = Duration.of_hour 2

let status_of_state result =
  let main_status =
    match result with
    | Ok status ->
        let msg =
          match status with
          | `No_changes -> "All good!"
          | `Csv_changes lst ->
              let m = status_file_list_to_strings lst in
              "One or more CSV files have been changed/added.\n" ^ m
              ^ "\nCheck the outputs folder for modified/new files."
          | `Config_changes lst ->
              let m = status_file_list_to_strings lst in
              "One or more configuration files have been changed.\n" ^ m
              ^ "\nCheck the outputs folder for modified/new files."
          | `Script_changes lst ->
              let m = status_file_list_to_strings lst in
              "One or more script has been changed.\n" ^ m
              ^ "\nCheck the outputs folder  for modified/new files."
          | `Multiple_changes lst ->
              let m = status_file_list_to_strings lst in
              "There have been multiple changes.\n" ^ m
              ^ "\nCheck the outputs folder for modified/new files."
        in
        ( Github.Api.CheckRunStatus.v ?text:(Some msg) (`Completed `Success),
          (`Success, msg) )
    | Error (`Active _) ->
        ( Github.Api.CheckRunStatus.v ?text:(Some "Running...") `InProgress,
          (`Running, "Pipeline is processing.") )
    | Error (`Msg m) ->
        ( Github.Api.CheckRunStatus.v ?text:(Some m) (`Completed (`Failure m)),
          (`Failed, m) )
  in
  main_status

let fetch_commit ~head () =
  let commit_id = Current.map Github.Api.Commit.id head in
  Git.fetch commit_id

let generate_new_jobs ~script_files src fc =
  let+ { script; input; config } =
    SCache.grab_script ~script_files ~fc src
    |> HCache.get_paths_and_hashes ~src ~fc
  in
  JCache.{ script; input; config }

let vv ~src ~rbw ~local_src ~github_commit () : Import.status Current.t =
  let inputs = "inputs"
  and outputs = "outputs"
  and scripts = "scripts"
  and default_config = "config" in
  Current.component "read files inside folders"
  |>
  let** _ = github_commit
  and* input_files =
    FCache.read_input_folder ~where:inputs ~config:default_config src
  and* script_files = FCache.read_folder ~label:scripts ~where:scripts src
  and* remote_origin, branch, website_branch = rbw in
  let input_files = input_files.files |> Current.return in
  let script_files = script_files.files in
  let+ _ =
    Current.list_map ~collapse_key:"jobs"
      (module FCache)
      (fun fc ->
        generate_new_jobs ~script_files src fc
        |> JCache.run_job ~local_src ~src ~inputs ~outputs)
      input_files
    |> GCache.add_commit_push ~label:"outputs" ~remote_origin ~branch
         ~commit_message:"OCurrent: Automatic push" src
  and+ s = GCache.diff ~branch:website_branch ~label:"check for changes" src in
  s

let v ~local ~installation () =
  let local_src = Git.Local.head_commit local in
  Github.Installation.repositories installation
  |> Current.list_iter ~collapse_key:"repos" (module Github.Api.Repo)
     @@ fun repo ->
     let refs = Github.Api.Repo.ci_refs ~staleness:(Duration.of_day 93) repo in
     refs
     |> Current.list_iter ~collapse_key:"refs" (module Github.Api.Commit)
        @@ fun head ->
        let src = fetch_commit ~head () in
        let rbw = GCache.remote_and_branch head in
        let r = vv ~src ~rbw ~local_src ~github_commit:head () in
        Current.component "finished"
        |>
        let** status = Current.state r and* _, _, website_branch = rbw in
        let status, s = status |> status_of_state in
        Status.set ~branch:website_branch ~s;
        status |> Current.return
        |> Github.Api.CheckRun.set_status head "Pipeline Execute"

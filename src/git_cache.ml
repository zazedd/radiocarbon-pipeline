open! Import
open Lwt.Infix

module Raw = struct
  module Commands = struct
    type t = { path : Fpath.t }

    type commands =
      [ `Commit
      | `Push of string
      | `Pull
      | `Add
      | `AddAll
      | `Status
      | `AddOrigin
      | `RmOrigin
      | `Fetch
      | `Config
      | `Checkout ]

    let id = "Pipeline-cache"

    let git_cmd_to_str cmd =
      match cmd with
      | `Commit -> "commit"
      | `Push branch -> "push to branch" ^ branch
      | `Pull -> "pull"
      | `Add -> "add"
      | `AddAll -> "add"
      | `Status -> "status"
      | `AddOrigin -> "remote add origin"
      | `RmOrigin -> "remote rm origin"
      | `Fetch -> "fetch"
      | `Config -> "config"
      | `Checkout -> "checkout"

    let git_cmd path cmd args =
      let path = Fpath.to_string path in
      match cmd with
      | `RmOrigin ->
          ("", Array.of_list ("git" :: [ "-C"; path; "remote"; "rm"; "origin" ]))
      | `AddOrigin ->
          ( "",
            Array.of_list
              (("git" :: [ "-C"; path; "remote"; "add"; "origin" ]) @ args) )
      | `Commit ->
          let l = List.fold_left (fun acc a -> acc ^ " " ^ a) "" args in
          ("", Array.of_list [ "git"; "-C"; path; "commit"; "-am"; l ])
      | `Push branch ->
          ( "",
            Array.of_list
              ([ "git"; "-C"; path; "push"; "-u"; "origin"; branch ] @ args) )
      | `Pull -> ("", Array.of_list ([ "git"; "-C"; path; "pull" ] @ args))
      | `AddAll -> ("", Array.of_list [ "git"; "-C"; path; "add"; "." ])
      | c ->
          let cmd = c |> git_cmd_to_str in
          ("", Array.of_list (("git" :: [ "-C"; path; cmd ]) @ args))

    module Key = struct
      type t = {
        commit : Git.Commit.t;
        remote_origin : string;
        branch : string;
        commit_message : string;
      }

      let to_json { commit; remote_origin; branch; commit_message } =
        `Assoc
          [
            ("commit hash", `String (Git.Commit.hash commit));
            ("remote_origin", `String remote_origin);
            ("branch", `String branch);
            ("commit_message", `String commit_message);
          ]

      let digest t = t |> to_json |> Yojson.Safe.to_string
      let pp f t = Yojson.Safe.pretty_print f (to_json t)
    end

    module Value = Current.Unit

    let exec_git_cmd ~job cmd msg =
      Current.Process.exec ~cancellable:false ~job cmd >|= function
      | Error m ->
          let m = match m with `Msg s -> s in
          let desc =
            Format.sprintf
              "Git command %s exited on error!\nERROR: %s\n%s\n               "
              (`Add |> git_cmd_to_str) m msg
          in
          Error (`Msg desc)
      | ok -> ok

    let exec_git ~cmd ~job ~path ~args ?(error_msg = "") () =
      let command = git_cmd path cmd args in
      exec_git_cmd ~job command error_msg

    let build { path } (job : Current.Job.t) (k : Key.t) :
        Value.t Current.or_error Lwt.t =
      let { commit = _; remote_origin; branch; commit_message } : Key.t = k in
      Current.Job.start ~level:Dangerous job >>= fun () ->
      exec_git ~cmd:`Fetch ~job ~path ~args:[ "--all" ] () >>!= fun _ ->
      exec_git ~cmd:`Checkout ~job ~path ~args:[ branch ] () >>!= fun _ ->
      exec_git ~cmd:`Config ~job ~path ~args:[ "pull.rebase"; "true" ] ()
      >>!= fun _ ->
      exec_git ~cmd:`Pull ~job ~path ~args:[ remote_origin; branch ] ()
      >>!= fun _ ->
      exec_git ~cmd:`AddAll ~job ~path ~args:[] () >>!= fun _ ->
      exec_git ~cmd:`RmOrigin ~job ~path ~args:[] () >>!= fun _ ->
      exec_git ~cmd:`AddOrigin ~job ~path ~args:[ remote_origin ] ()
      >>!= fun _ ->
      (* this can fail even if there is nothing to commit which isnt really a failure *)
      exec_git ~cmd:`Commit ~job ~path ~args:[ commit_message ] () >>= fun _ ->
      exec_git ~cmd:(`Push branch) ~job ~path ~args:[] ()

    let pp = Key.pp
    let auto_cancel = true
  end

  module Diff = struct
    type t = { path : Fpath.t; branch : string }

    let id = "git_diff cache"

    let git_cmd path =
      let path = Fpath.to_string path in
      ( "",
        Array.of_list
          ("git" :: [ "-C"; path; "diff"; "HEAD"; "HEAD^"; "--name-only" ]) )

    module Key = struct
      type t = { commit : Git.Commit.t }

      let to_json { commit } =
        `Assoc [ ("commit hash", `String (Git.Commit.hash commit)) ]

      let digest t = t |> to_json |> Yojson.Safe.to_string
      let pp f t = Yojson.Safe.pretty_print f (to_json t)
    end

    module Value = struct
      type t = Import.status

      let to_string = Import.status_to_string

      let from_string t =
        let aux l =
          l |> String.split_on_char '"' |> List.map String.trim
          |> List.filter (( <> ) "")
        in
        match String.split_on_char ':' t with
        | [ "No changes" ] -> `No_changes
        | [ "CSV changes. Files affected"; xs ] -> `Csv_changes (aux xs)
        | [ "Config changes. Files affected"; xs ] -> `Config_changes (aux xs)
        | [ "Script changes. Files affected"; xs ] -> `Script_changes (aux xs)
        | [ "Multiple changes. Files affected"; xs ] ->
            `Multiple_changes (aux xs)
        | _ -> assert false

      let pp f t = Fmt.string f (t |> to_string)

      let marshal t =
        let json = `Assoc [ ("value", `String (to_string t)) ] in
        Yojson.to_string json

      let unmarshal s =
        match Yojson.Safe.from_string s with
        | `Assoc [ ("value", `String v) ] -> from_string v
        | _ -> failwith "Failed to unmarshal files"
    end

    let exec_git_cmd ~branch ~job cmd =
      Current.Process.check_output ~cancellable:false ~job cmd >|= function
      | Error (`Msg s) ->
          let msg = Format.sprintf "ERROR: git diff errored out with %s@." s in
          Logs.err (fun f -> f "ERROR: git diff errored out with %s@." s);
          Error (`Msg msg)
      | Ok s -> (
          Logs.info (fun f -> f "OK: diff %s@." s);
          let split = s |> String.trim |> String.split_on_char '\n' in
          if List.length split = 0 then Ok `No_changes
          else
            let base_dirs_and_filenames =
              List.map
                (fun a ->
                  let p = Fpath.v a in
                  (p |> base_dir, p |> Fpath.base, a))
                split
            in
            let `Config_changes config, `Csv_changes csv, `Script_changes script
                =
              List.fold_left
                (fun ( `Config_changes config,
                       `Csv_changes csv,
                       `Script_changes script ) f ->
                  match f with
                  | dir, name, path
                    when dir = Fpath.v "inputs/" && name = Fpath.v "config" ->
                      ( `Config_changes (path :: config),
                        `Csv_changes csv,
                        `Script_changes script )
                  | dir, _, path when dir = Fpath.v "inputs/" ->
                      ( `Config_changes config,
                        `Csv_changes (path :: csv),
                        `Script_changes script )
                  | dir, _, path when dir = Fpath.v "scripts/" ->
                      ( `Config_changes config,
                        `Csv_changes csv,
                        `Script_changes (path :: script) )
                  | _ ->
                      ( `Config_changes config,
                        `Csv_changes csv,
                        `Script_changes script ))
                (`Config_changes [], `Csv_changes [], `Script_changes [])
                base_dirs_and_filenames
            in
            let s =
              ( `Running,
                Import.status_file_list_to_strings (csv @ config @ script) )
            in
            Status.set ~branch ~s;
            match (config, csv, script) with
            | [], [], [] -> Ok `No_changes
            | [], [], lst -> Ok (`Script_changes lst)
            | [], lst, [] -> Ok (`Csv_changes lst)
            | lst, [], [] -> Ok (`Config_changes lst)
            | configs, csvs, [] -> Ok (`Multiple_changes (csvs @ configs))
            | configs, [], scripts -> Ok (`Multiple_changes (configs @ scripts))
            | [], csvs, scripts -> Ok (`Multiple_changes (csvs @ scripts))
            | configs, csvs, scripts ->
                Ok (`Multiple_changes (csvs @ configs @ scripts)))

    let handle_context ~job commit fn = Current_git.with_checkout ~job commit fn

    let build { path; branch } (job : Current.Job.t) (k : Key.t) :
        Value.t Current.or_error Lwt.t =
      let { commit } : Key.t = k in
      Current.Job.start ~level:Dangerous job >>= fun () ->
      handle_context ~job commit @@ fun _ ->
      let cmd = git_cmd path in
      exec_git_cmd ~branch ~job cmd

    let pp = Key.pp
    let auto_cancel = true
  end
end

module GCommands = Current_cache.Make (Raw.Commands)

let add_commit_push ?schedule ~label ~remote_origin ~branch ~commit_message src
    d =
  let open Current.Syntax in
  Current.component "git: add, RmOrigin, AddOrigin, Commit, Push %a"
    Fmt.(string)
    label
  |>
  let> commit = src and> _ = d in
  let k = Raw.Commands.Key.{ commit; remote_origin; branch; commit_message } in
  let path = Git.Commit.repo commit in
  GCommands.get ?schedule { path } k

module GDiff = Current_cache.Make (Raw.Diff)

let diff ?schedule ~label ~branch github_src =
  let open Current.Syntax in
  Current.component "git: diff %a" Fmt.(string) label
  |>
  let> commit = github_src in
  let path = Git.Commit.repo commit in
  let k = Raw.Diff.Key.{ commit } in
  GDiff.get ?schedule { path; branch } k

let remote_and_branch github =
  let open Current.Syntax in
  let+ github_commit = github in
  let remote_origin =
    match Github.Api.Commit.pr_fork_owner_name github_commit with
    | Some ownername -> Format.sprintf "git@github.com:%s" ownername
    | None ->
        Format.asprintf "git@github.com:%a" Github.Api.Commit.pp_short
          github_commit
        |> String.split_on_char '\n' |> List.hd
  in
  let remote_origin = remote_origin ^ ".git" in
  let branch =
    match Github.Api.Commit.branch_name github_commit with
    | Some b -> b
    | None ->
        Option.value ~default:"main"
          (Github.Api.Commit.pr_fork_branch_name github_commit)
  in
  let website_branch =
    Github.Api.Commit.branch_name github_commit |> function
    | Some b -> b
    | None ->
        "pr_"
        ^ (Github.Api.Commit.pr_fork_owner_name github_commit |> Option.get)
        ^ (Github.Api.Commit.pr_fork_branch_name github_commit |> Option.get)
  in
  (remote_origin, branch, website_branch)

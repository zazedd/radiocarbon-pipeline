open! Import
open Lwt.Infix

module Raw = struct
  module Commands = struct
    type t = { path : Fpath.t }

    type commands =
      [ `Commit
      | `Push of string
      | `Add
      | `AddAll
      | `Status
      | `AddOrigin
      | `RmOrigin ]

    let id = "Pipeline-cache"

    let git_cmd_to_str cmd =
      match cmd with
      | `Commit -> "commit"
      | `Push branch -> "push to branch" ^ branch
      | `Add -> "add"
      | `AddAll -> "add"
      | `Status -> "status"
      | `AddOrigin -> "remote add origin"
      | `RmOrigin -> "remote rm origin"

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
            Array.of_list [ "git"; "-C"; path; "push"; "-u"; "origin"; branch ]
          )
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
      exec_git ~cmd:`AddAll ~job ~path ~args:[] () >>= fun _ ->
      exec_git ~cmd:`RmOrigin ~job ~path ~args:[] () >>= fun _ ->
      exec_git ~cmd:`AddOrigin ~job ~path ~args:[ remote_origin ] ()
      >>= fun _ ->
      exec_git ~cmd:`Commit ~job ~path ~args:[ commit_message ] () >>= fun _ ->
      exec_git ~cmd:(`Push branch) ~job ~path ~args:[] () >|= fun res -> res

    let pp = Key.pp
    let auto_cancel = true
  end

  module Diff = struct
    type t = { path : Fpath.t }

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

      let to_string = function
        | `No_changes -> "No changes"
        | `Csv_changes -> "Csv changes"
        | `Config_changes -> "Config changes"
        | `Script_changes -> "Script changes"
        | `Multiple_changes -> "Multiple changes"

      let from_string = function
        | "No changes" -> `No_changes
        | "Csv changes" -> `Csv_changes
        | "Config changes" -> `Config_changes
        | "Script changes" -> `Script_changes
        | "Multiple changes" -> `Multiple_changes
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

    let exec_git_cmd ~job cmd =
      Current.Process.check_output ~cancellable:false ~job cmd >|= function
      | Error (`Msg s) ->
          let msg = Format.sprintf "ERROR: git diff errored out with %s@." s in
          Logs.err (fun f -> f "ERROR: git diff errored out with %s@." s);
          Error (`Msg msg)
      | Ok s -> (
          Format.printf "OK: diff %s@." s;
          let split = s |> String.trim |> String.split_on_char '\n' in
          if List.length split = 0 then Ok `No_changes
          else
            let base_dirs_and_filenames =
              List.map
                (fun a ->
                  let p = Fpath.v a in
                  (p |> base_dir, p |> Fpath.base))
                split
            in
            let changes =
              List.map
                (fun f ->
                  match f with
                  | dir, name
                    when dir = Fpath.v "inputs/" && name = Fpath.v "config" ->
                      `Config_changes
                  | dir, _ when dir = Fpath.v "inputs/" -> `Csv_changes
                  | dir, _ when dir = Fpath.v "scripts/" -> `Script_changes
                  | _ -> `No_changes)
                base_dirs_and_filenames
            in
            let has_config =
              List.find_opt (fun a -> a = `Config_changes) changes
            in
            let has_csv = List.find_opt (fun a -> a = `Csv_changes) changes in
            let has_script =
              List.find_opt (fun a -> a = `Script_changes) changes
            in
            match (has_config, has_csv, has_script) with
            | Some _, Some _, Some _
            | None, Some _, Some _
            | Some _, None, Some _
            | Some _, Some _, None ->
                Ok `Multiple_changes
            | Some _, None, None -> Ok `Config_changes
            | None, Some _, None -> Ok `Csv_changes
            | None, None, Some _ -> Ok `Script_changes
            | None, None, None -> Ok `No_changes)

    let handle_context ~job commit fn = Current_git.with_checkout ~job commit fn

    let build { path } (job : Current.Job.t) (k : Key.t) :
        Value.t Current.or_error Lwt.t =
      let { commit } : Key.t = k in
      Current.Job.start ~level:Dangerous job >>= fun () ->
      handle_context ~job commit @@ fun _ ->
      let cmd = git_cmd path in
      exec_git_cmd ~job cmd >|= fun res -> res

    let pp = Key.pp
    let auto_cancel = true
  end

  module RemoteBranch = struct
    type t = unit

    let id = "git_remote_branch cache"

    module Key = struct
      type t = { github_commit : Github.Api.Commit.t }

      let to_json { github_commit } =
        `Assoc
          [ ("commit hash", `String (Github.Api.Commit.hash github_commit)) ]

      let digest t = t |> to_json |> Yojson.Safe.to_string
      let pp f t = Yojson.Safe.pretty_print f (to_json t)
    end

    module Value = struct
      type t = { remote_origin : string; branch : string }

      let marshal t =
        let { remote_origin; branch } = t in
        let json =
          `Assoc
            [
              ("remote_origin", `String remote_origin);
              ("branch", `String branch);
            ]
        in
        Yojson.to_string json

      let unmarshal s =
        match Yojson.Safe.from_string s with
        | `Assoc
            [
              ("remote_origin", `String remote_origin);
              ("branch", `String branch);
            ] ->
            { remote_origin; branch }
        | _ -> failwith "Failed to unmarshal files"
    end

    let build _ (job : Current.Job.t) (k : Key.t) :
        Value.t Current.or_error Lwt.t =
      let { github_commit } : Key.t = k in
      Current.Job.start ~level:Dangerous job >>= fun () ->
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
      Ok Value.{ remote_origin; branch } |> Lwt.return >|= fun res -> res

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

let diff ?schedule ~label github_src =
  let open Current.Syntax in
  Current.component "git: diff %a" Fmt.(string) label
  |>
  let> commit = github_src in
  let path = Git.Commit.repo commit in
  let k = Raw.Diff.Key.{ commit } in
  GDiff.get ?schedule { path } k

module GRemoteBranch = Current_cache.Make (Raw.RemoteBranch)

let remote_and_branch ?schedule github =
  let open Current.Syntax in
  Current.component "git: remote origin and branch"
  |>
  let> github_commit = github in
  let k = Raw.RemoteBranch.Key.{ github_commit } in
  GRemoteBranch.get ?schedule () k

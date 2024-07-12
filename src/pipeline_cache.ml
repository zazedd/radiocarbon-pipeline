open! Import
open Lwt.Infix

module Raw = struct
  module Pipeline_cache = struct
    type t = { path : Fpath.t; output_files : string list }

    type commands =
      [ `Commit | `Push | `Add | `AddAll | `Status | `AddOrigin | `RmOrigin ]

    let id = "Pipeline-cache"

    let git_cmd_to_str cmd =
      match cmd with
      | `Commit -> "commit"
      | `Push -> "push"
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
      | `Push ->
          ( "",
            Array.of_list [ "git"; "-C"; path; "push"; "-u"; "origin"; "main" ]
          )
      | `AddAll -> ("", Array.of_list [ "git"; "-C"; path; "add"; "." ])
      | c ->
          let cmd = c |> git_cmd_to_str in
          ("", Array.of_list (("git" :: [ "-C"; path; cmd ]) @ args))

    module Key = struct
      type t = {
        github_commit : Github.Api.Commit.t;
        nix_args : string list list;
        remote_origin : string;
        commit_message : string;
      }

      let to_json { github_commit; nix_args; remote_origin; commit_message } =
        `Assoc
          [
            ( "github_commit hash",
              `String (Github.Api.Commit.hash github_commit) );
            ("nix_args", [%derive.to_yojson: string list list] nix_args);
            ("remote_origin", `String remote_origin);
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

    let build { path; output_files = _ } (job : Current.Job.t) (k : Key.t) :
        Value.t Current.or_error Lwt.t =
      let { github_commit = _; nix_args = _; remote_origin; commit_message } :
          Key.t =
        k
      in
      Current.Job.start ~level:Dangerous job >>= fun () ->
      (* exec_git ~cmd:`Add ~job ~path ~args:output_files () >>= fun _ -> *)
      exec_git ~cmd:`AddAll ~job ~path ~args:[] () >>= fun _ ->
      exec_git ~cmd:`RmOrigin ~job ~path ~args:[] () >>= fun _ ->
      exec_git ~cmd:`AddOrigin ~job ~path ~args:[ remote_origin ] ()
      >>= fun _ ->
      exec_git ~cmd:`Commit ~job ~path ~args:[ commit_message ] () >>= fun _ ->
      exec_git ~cmd:`Push ~job ~path ~args:[] () >|= fun res -> res

    let pp = Key.pp
    let auto_cancel = true
  end
end

module Pipeline = Current_cache.Make (Raw.Pipeline_cache)

let run ?schedule ~label ~path ~output_files ~github_commit ~nix_args
    ~remote_origin ~commit_message d =
  let open Current.Syntax in
  let k =
    Raw.Pipeline_cache.Key.
      { github_commit; nix_args; remote_origin; commit_message }
  in
  Current.component "git: add, RmOrigin, AddOrigin, Commit, Push %a"
    Fmt.(string)
    label
  |>
  let> _ = d in
  Pipeline.get ?schedule { path; output_files } k

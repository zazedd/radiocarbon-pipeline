open! Import
open Lwt.Infix

module Raw = struct
  module Git_hash = struct
    type t = { commit : Git.Commit.t }

    let auto_cancel = true
    let id = "git-hashes"

    module Key = struct
      type t = { dir : Fpath.t }

      let to_json t = `Assoc [ ("dir", `String (t.dir |> Fpath.to_string)) ]
      let digest t = to_json t |> Yojson.Safe.to_string
    end

    let pp ppf t = Yojson.pp ppf (Key.to_json t)

    module Value = struct
      type t = { files : (Fpath.t * string) list } (* filename * digest *)

      let marshal t =
        let json =
          `Assoc
            [
              ( "files",
                `List
                  (List.map
                     (fun f ->
                       let fn = fst f and dg = snd f in
                       `String (Fpath.to_string fn ^ "&" ^ dg))
                     t.files) );
            ]
        in
        Yojson.to_string json

      let unmarshal s =
        match Yojson.Safe.from_string s with
        | `Assoc [ ("files", `List f) ] ->
            let files =
              List.map
                (fun m ->
                  let m = match m with `String m -> m | _ -> assert false in
                  match String.split_on_char '&' m with
                  | [ fn; dg ] -> (Fpath.v fn, dg)
                  | _ -> assert false)
                f
            in
            { files }
        | _ -> failwith "Failed to unmarshal files"
    end

    let digest_file path =
      Lwt_io.(open_file ~mode:Input (Fpath.to_string path)) >>= fun file ->
      Lwt.finalize
        (fun () ->
          Lwt_io.read file >|= fun content ->
          Digestif.SHA512.digest_string content |> Digestif.SHA512.to_hex)
        (fun () -> Lwt_io.close file)

    let or_raise = function Ok v -> v | Error (`Msg m) -> failwith m

    let build { commit } (job : Current.Job.t) (k : Key.t) :
        Value.t Current.or_error Lwt.t =
      Current.Job.start ~level:Harmless job >>= fun () ->
      Current_git.with_checkout ~job commit @@ fun dir ->
      let paths = Bos.OS.Dir.contents Fpath.(dir // k.dir) |> Result.get_ok in
      Current.Job.log job "Directory %a contains %a" Fpath.pp k.dir
        Fmt.(list Fpath.pp)
        paths;
      Lwt_list.map_p
        (fun path -> digest_file path >|= fun digest -> (path, digest))
        paths
      >>= fun l -> Lwt.return (Ok Value.{ files = l })
  end

  module Git_commands = struct
    type t = { path : Fpath.t; github_commit : Github.Api.Commit.t Current.t }

    let id = "git-commands"

    let command_to_str cmd =
      match cmd with
      | `Commit_push -> "commit and push"
      | `Push -> "push"
      | `Add -> "add"
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
      | `Commit_push ->
          let l =
            "git -C " ^ path ^ " commit "
            ^ List.fold_left (fun acc a -> acc ^ " " ^ a) "" args
            ^ " && git -C " ^ path ^ " push -u origin main"
          in
          ("", Array.of_list ([ "bash"; "-c" ] @ [ l ]))
      | c ->
          let cmd = c |> command_to_str in
          ("", Array.of_list (("git" :: [ "-C"; path; cmd ]) @ args))

    module Key = struct
      type t = {
        command :
          [ `Commit_push | `Push | `Add | `Status | `AddOrigin | `RmOrigin ];
        args : string list;
      }

      let to_json { command; args } =
        `Assoc
          [
            ("command", `String (command |> command_to_str));
            ("args", [%derive.to_yojson: string list] args);
          ]

      let digest t = t |> to_json |> Yojson.Safe.to_string
      let pp f t = Yojson.Safe.pretty_print f (to_json t)
    end

    module Value = Current.Unit

    let build { path; github_commit = _g } (job : Current.Job.t) (k : Key.t) :
        Value.t Current.or_error Lwt.t =
      let { Key.command; args } = k in
      Current.Job.start ~level:Dangerous job >>= fun () ->
      let cmd = git_cmd path command args in
      (Current.Process.exec ~cancellable:false ~job cmd >|= function
       | Error m ->
           let m = match m with `Msg s -> s in
           let desc =
             Format.sprintf
               "Git command %s exited on error!\n\
                ERROR: %s\n\
                Was an output file already computed for the inputs provided?"
               (command |> command_to_str)
               m
           in
           Error (`Msg desc)
       | ok -> ok)
      >|= fun res -> res

    let pp = Key.pp
    let auto_cancel = true
  end
end

module GitCmds = Current_cache.Make (Raw.Git_commands)

let commit_push ?schedule ~label ~path ~github_commit args d =
  let open Current.Syntax in
  let k = Raw.Git_commands.Key.{ command = `Commit_push; args } in
  Current.component "git: commit and push %a" Fmt.(string) label
  |>
  let> _ = d in
  GitCmds.invalidate k;
  let res = GitCmds.get ?schedule { path; github_commit } k in
  GitCmds.invalidate k;
  res

let push ?schedule ~label ~path ~github_commit args =
  let open Current.Syntax in
  Current.component "git: push %a" Fmt.(string) label
  |>
  let> _ = () |> Current.return in
  GitCmds.get ?schedule { path; github_commit } { command = `Push; args }

let add ?schedule ~label ~path ~github_commit args d =
  let open Current.Syntax in
  let k = Raw.Git_commands.Key.{ command = `Add; args } in
  Current.component "git: add %a" Fmt.(string) label
  |>
  let> _ = d in
  GitCmds.invalidate k;
  let res = GitCmds.get ?schedule { path; github_commit } k in
  GitCmds.invalidate k;
  res

let status ?schedule ~label ~path ~github_commit () =
  let open Current.Syntax in
  Current.component "git: status %a" Fmt.(string) label
  |>
  let> _ = () |> Current.return in
  GitCmds.get ?schedule { path; github_commit } { command = `Status; args = [] }

let add_origin ?schedule ~label ~path ~github_commit arg d =
  let open Current.Syntax in
  let k = Raw.Git_commands.Key.{ command = `AddOrigin; args = [ arg ] } in
  Current.component "git: set origin %a" Fmt.(string) label
  |>
  let> _ = d in
  GitCmds.invalidate k;
  let res = GitCmds.get ?schedule { path; github_commit } k in
  GitCmds.invalidate k;
  res

let rm_origin ?schedule ~label ~path ~github_commit d =
  let open Current.Syntax in
  let k = Raw.Git_commands.Key.{ command = `RmOrigin; args = [] } in
  Current.component "git: rm origin %a" Fmt.(string) label
  |>
  let> _ = d in
  GitCmds.invalidate k;
  let res = GitCmds.get ?schedule { path; github_commit } k in
  GitCmds.invalidate k;
  res

module TestC = Current_cache.Make (Raw.Git_hash)

(* fix prefixes, /var/folders/.../inputs/... != inputs/... *)
let fix_prefixes fl dir =
  List.map
    (fun (p, d) ->
      let file = Fpath.base p in
      (Fpath.(dir // file), d))
    fl

let call_cache ?schedule commit dir d : Raw.Git_hash.Value.t Current.t =
  let open Current.Syntax in
  let k = Raw.Git_hash.Key.{ dir } in
  Current.component "grabbing hashes inside %a/" Fpath.pp (Fpath.base dir)
  |> let> commit = commit and> _ = d in
     TestC.get ?schedule { commit } k

let grab_hashes commit ~test (new_hash : Raw.Git_hash.Value.t Current.t) dir d =
  let open Current.Syntax in
  let k = Raw.Git_hash.Key.{ dir } in
  let old = call_cache commit dir d in
  let+ old_hashes = old and+ new_hashes = new_hash in
  let old_hashes = fix_prefixes old_hashes.files dir in
  let new_hashes = fix_prefixes new_hashes.files dir in
  if old_hashes <> new_hashes then (
    let changed_and_new =
      List.filter (fun file -> List.mem file old_hashes |> not) new_hashes
    in
    TestC.invalidate k;
    Some changed_and_new)
  else None

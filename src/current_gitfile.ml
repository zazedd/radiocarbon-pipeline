open! Import
open Lwt.Infix
module Git = Current_git

module Raw = struct
  module Git_file = struct
    type t = No_context

    let auto_cancel = true
    let id = "git-file"

    module Key = struct
      type t = { commit : Git.Commit.t; files : Fpath.t list }

      let to_json t =
        `Assoc
          [
            ("commit", `String (Git.Commit.hash t.commit));
            ( "files",
              `List
                (List.map (fun file -> `String (Fpath.to_string file)) t.files)
            );
          ]

      let digest t = to_json t |> Yojson.Safe.to_string
    end

    let pp ppf t = Yojson.pp ppf (Key.to_json t)

    module Value = struct
      type t = (Fpath.t * string) list

      let marshal ts =
        `List
          (List.map
             (fun (p, v) -> `List [ `String (Fpath.to_string p); `String v ])
             ts)
        |> Yojson.Safe.to_string

      let unmarshal s =
        match Yojson.Safe.from_string s with
        | `List lst ->
            List.map
              (function
                | `List [ `String p; `String c ] ->
                    let f = Fpath.of_string p |> or_raise in
                    (f, c)
                | _ -> failwith "Failed to unmarshal files")
              lst
        | _ -> failwith "Failed to unmarshal files"
    end

    let or_raise = function Ok v -> v | Error (`Msg m) -> failwith m

    let build No_context (job : Current.Job.t) (k : Key.t) :
        Value.t Current.or_error Lwt.t =
      Current.Job.start ~level:Harmless job >>= fun () ->
      Current_git.with_checkout ~job k.commit @@ fun dir ->
      let paths = List.map (fun file -> Fpath.(dir // file)) k.files in
      let contents =
        try
          Ok
            (List.map
               (fun path -> (path, Bos.OS.File.read path |> or_raise))
               paths)
        with Failure msg -> Error (`Msg msg)
      in
      Lwt.return contents
  end

  module Git_dir_contents = struct
    type t = No_context

    let auto_cancel = true
    let id = "git-file"

    module Key = struct
      type t = { commit : Git.Commit.t; directory : Fpath.t }

      let to_json t =
        `Assoc
          [
            ("commit", `String (Git.Commit.hash t.commit));
            ("directory", `String (Fpath.to_string t.directory));
          ]

      let digest t = to_json t |> Yojson.Safe.to_string
    end

    let pp ppf t = Yojson.pp ppf (Key.to_json t)

    module Value = struct
      type t = (Fpath.t * string) list

      let marshal ts =
        `List
          (List.map
             (fun (p, v) -> `List [ `String (Fpath.to_string p); `String v ])
             ts)
        |> Yojson.Safe.to_string

      let unmarshal s =
        match Yojson.Safe.from_string s with
        | `List lst ->
            List.map
              (function
                | `List [ `String p; `String c ] ->
                    let f = Fpath.of_string p |> or_raise in
                    (f, c)
                | _ -> failwith "Failed to unmarshal files")
              lst
        | _ -> failwith "Failed to unmarshal files"
    end

    let or_raise = function Ok v -> v | Error (`Msg m) -> failwith m

    let build No_context (job : Current.Job.t) (k : Key.t) :
        Value.t Current.or_error Lwt.t =
      Current.Job.start ~level:Harmless job >>= fun () ->
      Current_git.with_checkout ~job k.commit @@ fun dir ->
      Lwt.return @@ Bos.OS.Dir.contents Fpath.(dir // k.directory)
      >>!= fun paths ->
      let contents =
        try
          Ok
            (List.map
               (fun path ->
                 ( path,
                   Bos.OS.File.read path |> or_raise
                   |> Digestif.SHA512.digest_string |> Digestif.SHA512.to_hex ))
               paths)
        with Failure msg -> Error (`Msg msg)
      in
      Lwt.return contents
  end

  module Git_dir = struct
    type t = No_context

    let auto_cancel = true
    let id = "git-file"

    module Key = struct
      type t = { commit : Git.Commit.t; dir : Fpath.t }

      let to_json t =
        `Assoc
          [
            ("commit", `String (Git.Commit.hash t.commit));
            ("dir", `String (Fpath.to_string t.dir));
          ]

      let digest t = to_json t |> Yojson.Safe.to_string
    end

    let pp ppf t = Yojson.pp ppf (Key.to_json t)

    module Value = struct
      (* Directory + Digest *)
      type t = { dir : Fpath.t; files : Fpath.t list; digest : string }

      let marshal t =
        let json =
          `Assoc
            [
              ("dir", `String (Fpath.to_string t.dir));
              ( "files",
                `List (List.map (fun f -> `String (Fpath.to_string f)) t.files)
              );
              ("digest", `String t.digest);
            ]
        in
        Yojson.to_string json

      let unmarshal s =
        match Yojson.Safe.from_string s with
        | `Assoc
            [
              ("dir", `String dir);
              ("files", `List files);
              ("digest", `String digest);
            ] ->
            let dir = Fpath.of_string dir |> or_raise in
            let files =
              List.map
                (function
                  | `String s -> Fpath.of_string s |> or_raise
                  | _ -> failwith "Failed to unmarshal files")
                files
            in
            { dir; files; digest }
        | _ -> failwith "Failed to unmarshal files"
    end

    let digest_file path =
      Lwt_io.(open_file ~mode:Input (Fpath.to_string path)) >>= fun file ->
      Lwt.finalize
        (fun () ->
          Lwt_io.read file >|= fun content ->
          Digestif.SHA256.digest_string content |> Digestif.SHA256.to_hex)
        (fun () -> Lwt_io.close file)

    let mv ~cancellable ~job ~src ~dst =
      let cmd = [| "mv"; Fpath.to_string src; Fpath.to_string dst |] in
      Current.Process.exec ~cancellable ~job ("", cmd)

    let build No_context (job : Current.Job.t) (k : Key.t) :
        Value.t Current.or_error Lwt.t =
      let git_dir = Current.state_dir "git-dir" in
      Current.Job.start ~level:Harmless job >>= fun () ->
      Current_git.with_checkout ~job k.commit @@ fun dir ->
      let paths = Bos.OS.Dir.contents Fpath.(dir // k.dir) |> Result.get_ok in
      Current.Job.log job "Directory %a contains %a" Fpath.pp k.dir
        Fmt.(list Fpath.pp)
        paths;
      Lwt_list.map_p (fun path -> digest_file path >|= fun d -> (path, d)) paths
      >>= fun output ->
      let digest =
        List.fold_left (fun acc (_, d) -> Digest.string (acc ^ d)) "" output
        |> Digest.to_hex
      in
      let dst_dir =
        Fpath.of_string (Fpath.to_string k.dir ^ "-" ^ Git.Commit.hash k.commit)
        |> Result.get_ok
      in
      mv ~cancellable:true ~job
        ~src:Fpath.(dir // k.dir)
        ~dst:Fpath.(git_dir // dst_dir)
      >>!= fun () ->
      let v =
        Value.
          {
            digest;
            dir = Fpath.(git_dir // dst_dir);
            files = List.map fst output;
          }
      in
      Lwt.return (Ok v)
  end

  module Git_commands = struct
    type t = No_context

    let id = "git-commands"

    let command_to_str cmd =
      match cmd with
      | `Commit_push -> "commit and push"
      | `Push -> "push"
      | `Add -> "add"
      | `Status -> "status"

    let git_cmd cmd args =
      match cmd with
      | `Commit_push ->
          let l =
            "git commit"
            ^ List.fold_left (fun acc a -> acc ^ " " ^ a) "" args
            ^ " && git push -u"
          in
          ("", Array.of_list ([ "bash"; "-c" ] @ [ l ]))
      | c ->
          let cmd = c |> command_to_str in
          ("", Array.of_list (("git" :: [ cmd ]) @ args))

    module Key = struct
      type t = {
        command : [ `Commit_push | `Push | `Add | `Status ];
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

    let build No_context (job : Current.Job.t) (k : Key.t) :
        Value.t Current.or_error Lwt.t =
      let { Key.command; args } = k in
      Current.Job.start ~level:Dangerous job >>= fun () ->
      let cmd = git_cmd command args in
      Current.Process.exec ~cancellable:true ~job cmd

    let pp = Key.pp
    let auto_cancel = true
  end
end

module GitFileC = Current_cache.Make (Raw.Git_file)
module GitDirC = Current_cache.Make (Raw.Git_dir_contents)
module GitCmds = Current_cache.Make (Raw.Git_commands)

let raw_git_file ?schedule commit files =
  let key = Raw.Git_file.Key.{ commit; files } in
  GitFileC.get ?schedule No_context key

let raw_git_dir ?schedule commit directory =
  let key = Raw.Git_dir_contents.Key.{ commit; directory } in
  GitDirC.get ?schedule No_context key

let contents ?schedule commit files =
  let open Current.Syntax in
  Current.component "read %a" (Fmt.list Fpath.pp) files
  |> let> commit = commit in
     raw_git_file ?schedule commit files

let directory_contents_hashes ?schedule commit directory ~label =
  let open Current.Syntax in
  Current.component "read %a" Fmt.(string) label
  |> let> commit = commit in
     raw_git_dir ?schedule commit directory

let commit_push ?schedule ~label args =
  let open Current.Syntax in
  Current.component "git: commit and push %a" Fmt.(string) label
  |>
  let> _ = () |> Current.return in
  GitCmds.get ?schedule No_context { command = `Commit_push; args }

let push ?schedule ~label args =
  let open Current.Syntax in
  Current.component "git: push %a" Fmt.(string) label
  |>
  let> _ = () |> Current.return in
  GitCmds.get ?schedule No_context { command = `Push; args }

let add ?schedule ~label args =
  let open Current.Syntax in
  Current.component "git: add %a" Fmt.(string) label
  |>
  let> _ = () |> Current.return in
  GitCmds.get ?schedule No_context { command = `Add; args }

let status ?schedule ~label () =
  let open Current.Syntax in
  Current.component "git: status %a" Fmt.(string) label
  |>
  let> _ = () |> Current.return in
  GitCmds.get ?schedule No_context { command = `Status; args = [] }

module GitDirectoryC = Current_cache.Make (Raw.Git_dir)

let raw_git_dir ?schedule commit dir =
  let key = Raw.Git_dir.Key.{ commit; dir } in
  GitDirectoryC.get ?schedule No_context key

let directory ?schedule commit dir =
  let open Current.Syntax in
  Current.component "directory %a" Fpath.pp dir
  |> let> commit = commit in
     raw_git_dir ?schedule commit dir

open Lwt.Infix
module Git = Current_git

type t = { new_commit : Git.Commit.t }

let id = "repo-cache"

module Key = struct
  type t = { commit : Git.Commit.t option }

  let to_json { commit } =
    match commit with
    | None -> `Assoc [ ("commit", `String "no commit yet") ]
    | Some commit -> `Assoc [ ("commit", `String (Git.Commit.marshal commit)) ]

  let digest t = Yojson.Safe.to_string (to_json t)
  let pp f t = Yojson.Safe.pretty_print f (to_json t)
end

module Value = struct
  type file = { name : string; hash : string }

  (* file names and hashes of the inputs/ directory *)
  type t = file list

  let file_names_to_strings f =
    List.fold_left
      (fun (name_acc, hash_acc) a ->
        (name_acc ^ "\n" ^ a.name, hash_acc ^ "\n" ^ a.hash))
      ("", "") f

  let to_json files =
    let names, hashes = file_names_to_strings files in
    `Assoc [ ("file-names", `String names); ("file-hashes", `String hashes) ]

  let marshal t = Yojson.Safe.to_string (to_json t)

  let unmarshal json_str =
    let open Yojson.Safe.Util in
    let json = Yojson.Safe.from_string json_str in
    let names_str = json |> member "file-names" |> to_string in
    let hashes_str = json |> member "file-hashes" |> to_string in
    let names = String.split_on_char '\n' names_str |> List.tl in
    let hashes = String.split_on_char '\n' hashes_str |> List.tl in
    List.map2 (fun name hash -> { name; hash }) names hashes

  let pp f t = Yojson.Safe.pretty_print f (to_json t)
end

(*module Outcome = struct*)
(*  include Current.String*)
(*end*)

(*let h ~job = Current_cache.Db.query ~job_prefix:(Current.Job.id job) ()*)
(**)
(*let run ~c ~job ~key ~value =*)
(*  Current.Job.start job ~level:Current.Level.Dangerous >>= fun () ->*)
(*  let commit = key in*)
(*  Git.with_checkout ~job commit @@ fun dir ->*)
(*  (*Prometheus.Gauge.inc_one Metrics.docker_push_events;*)*)
(*  (Current.Process.exec ~cancellable:true ~job (h ~job) >>= function*)
(*   | Error _ as e -> Lwt.return e*)
(*   | Ok () ->*)
(*       let { Key.tag; docker_context } = key in*)
(*       (match auth with*)
(*       | None -> Lwt.return (Ok ())*)
(*       | Some (user, password) ->*)
(*           let cmd = Cmd.login ~docker_context user in*)
(*           Current.Process.exec ~cancellable:true ~job ~stdin:password cmd)*)
(*       >>!= fun () ->*)
(*       let cmd = Cmd.docker ~docker_context [ "push"; tag ] in*)
(*       Current.Process.exec ~cancellable:true ~job cmd >>!= fun () ->*)
(*       let cmd =*)
(*         Cmd.docker ~docker_context*)
(*           [ "image"; "inspect"; tag; "-f"; "{{index .RepoDigests 0}}" ]*)
(*       in*)
(*       Current.Process.check_output ~cancellable:false ~job cmd*)
(*       >|= Stdlib.Result.map @@ fun id ->*)
(*           let repo_id = String.trim id in*)
(*           Current.Job.log job "Pushed %S -> %S" tag repo_id;*)
(*           repo_id)*)
(*  >|= fun res ->*)
(*  (*Prometheus.Gauge.inc_one Metrics.docker_push_events;*)*)
(*  res*)

let build t job (key : Key.t) =
  Current.Job.start job ~level:Current.Level.Dangerous >>= fun () ->
  let new_commit = t.new_commit in
  let commit : Git.Commit.t option = key.commit in
  let commit = Option.value ~default:new_commit commit in
  Git.with_checkout ~job commit @@ fun dir ->
  Format.printf "new_commit hash: %s@." (new_commit |> Git.Commit.hash);
  Format.printf "last_commit hash: %s@." (commit |> Git.Commit.hash);
  (Current.Process.exec ~cancellable:true ~job ("", [| "echo"; "hello" |])
   >|= function
   | Error _ as e -> e
   | Ok () ->
       Format.printf "folder: %s@." (dir |> Fpath.to_string);
       Bos.OS.File.read Fpath.(dir / ".git/refs/heads/nix")
       |> Stdlib.Result.map @@ fun c ->
          Format.printf "previous commit hash: %s@." c;
          let open Value in
          let v : Value.t = [ { name = "test"; hash = "test" } ] in
          v)
  >|= fun res -> res

let pp = Key.pp
let auto_cancel = true

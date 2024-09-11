open! Import
open Lwt.Infix

(* filepath * hash *)
type job = {
  script : Fpath.t * string;
  input : Fpath.t * string;
  config : Fpath.t * string;
}

let compare_job j1 j2 = compare j1.input j2.input

type t = job [@@deriving ord]

let pp f t =
  Fmt.string f
    (Format.sprintf "in: %s, script: %s, config: %s"
       (t.input |> fst |> file_path (Fpath.v "inputs"))
       (t.script |> fst |> file_path (Fpath.v "scripts"))
       (t.config |> fst |> file_path (Fpath.v "inputs")))

module Raw = struct
  let handle_context ~job commit fn = Current_git.with_checkout ~job commit fn

  module Cmd = struct
    let shell args =
      ("", Array.of_list (("nix" :: [ "develop"; "--verbose" ]) @ args))

    let transform_args args =
      let args =
        List.mapi
          (fun i arg_list ->
            if i <> List.length args - 1 then arg_list @ [ "&&" ] else arg_list)
          args
        |> List.fold_left
             (fun acc arg_list ->
               acc
               ^ List.fold_left (fun acc2 arg -> acc2 ^ " " ^ arg) "" arg_list)
             ""
      in
      [ "-c"; "bash"; "-c"; args ]
  end

  module Jobs = struct
    type t = {
      local_commit : Git.Commit.t;
      inputs : Fpath.t;
      outputs : Fpath.t;
    }

    let id = "jobs-cache"

    module Key = struct
      (* these are all absolute paths to the files *)
      type t = job

      let to_json t =
        let s, sh = t.script and i, ih = t.input and c, ch = t.config in
        `Assoc
          [
            ("script", `String (s |> Fpath.to_string));
            ("script_hash", `String sh);
            ("input", `String (i |> Fpath.to_string));
            ("input_hash", `String ih);
            ("config", `String (c |> Fpath.to_string));
            ("config_hash", `String ch);
          ]

      let digest t = to_json t |> Yojson.Safe.to_string
      let pp f t = Yojson.Safe.pretty_print f (to_json t)
    end

    module Value = struct
      type t = { files : (Fpath.t * string) list } [@@deriving ord]
      (* file path * content *)

      let file_pp f t = Fpath.pp f (t |> fst)
      let pp f t = Format.pp_print_list file_pp f t.files

      let marshal t =
        let json =
          `Assoc
            [
              ( "files",
                `List
                  (List.map
                     (fun f ->
                       let fn = fst f and ct = snd f in
                       `List (`String (Fpath.to_string fn) :: [ `String ct ]))
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
                  let m = match m with `List m -> m | _ -> assert false in
                  match m with
                  | [ `String fn; `String ct ] -> (Fpath.v fn, ct)
                  | _ -> assert false)
                f
            in
            { files }
        | _ -> failwith "Failed to unmarshal files"
    end

    (* this will do a bunch of stuff
       it grab the current job, and then:
       1: create the csv output file name, relative and absolute,
       2: create a temporary diretory to store extra files (like pdfs) created by the nix shell
       3: read those files and store them as the value of our cache
       4: write them to the correct file destinations on the remote repository
       the git cache will then push them over
    *)
    let build { local_commit; inputs; outputs } (job : Current.Job.t)
        (k : Key.t) : Value.t Current.or_error Lwt.t =
      let { script; input; config } : Key.t = k in
      (* step 1 *)
      let s, _ = script and i, _ = input and c, _ = config in
      let script_no_ext = s |> Fpath.base |> Fpath.rem_ext |> Fpath.to_string in
      let current_time = Unix.localtime (Unix.time ()) in
      let formatted_time =
        Printf.sprintf "%02d-%02d-%04d@%02d:%02d:%02d" current_time.Unix.tm_mday
          (current_time.Unix.tm_mon + 1) (* tm_mon is zero-indexed *)
          (current_time.Unix.tm_year + 1900) (* tm_year is years since 1900 *)
          current_time.Unix.tm_hour current_time.Unix.tm_min
          current_time.Unix.tm_sec
      in
      let output_file_name =
        i |> Fpath.base |> Fpath.rem_ext
        |> Fpath.add_ext script_no_ext
        |> Fpath.add_ext formatted_time
        |> Fpath.add_ext "csv"
      in
      let input_directory =
        i |> Fpath.split_base |> fst |> Fpath.rem_empty_seg
      in
      let abs_output_file_dir =
        let suffix =
          get_suffix input_directory inputs output_file_name
          |> Fpath.split_base |> fst
        in
        Fpath.(outputs // suffix)
      in
      (* step 2 *)
      Current.Job.start ~level:Dangerous job >>= fun () ->
      Current.Process.with_tmpdir @@ fun dir ->
      let tmp_output_file = Fpath.(dir // output_file_name) in
      let args =
        [
          [ "cd"; dir |> Fpath.to_string ];
          [
            "Rscript";
            s |> Fpath.to_string;
            i |> Fpath.to_string;
            tmp_output_file |> Fpath.to_string;
            c |> Fpath.to_string;
          ];
        ]
        |> Cmd.transform_args
      in
      handle_context ~job local_commit @@ fun nix_dir ->
      let flake_file = ".#" in
      let nix_cmd = Cmd.shell ([ flake_file ] @ args) in
      begin
        (* step 3 *)
        Current.Process.exec ~cancellable:false ~job nix_cmd >|= function
        | Error _ as e -> e
        | Ok () ->
            Bos.OS.File.read Fpath.(nix_dir / "flake.lock")
            |> Stdlib.Result.map @@ fun lock_content ->
               Logs.info (fun f ->
                   f "Built Nix Derivation. Lock file: %s" lock_content);
               let files =
                 Bos.OS.Dir.fold_contents
                   (fun file acc ->
                     (file, Bos.OS.File.read file |> Result.get_ok) :: acc)
                   [] dir
                 |> Result.get_ok
               in
               Value.{ files }
      end
      (* step 4 *)
      >|= function
      | Error _ as e -> e
      | Ok { files } -> begin
          let folder = abs_output_file_dir in
          Bos.OS.Dir.create folder |> function
          | Error _ as e -> e
          | Ok _ ->
              List.iter
                (fun (file, content) ->
                  let file_name =
                    Fpath.(abs_output_file_dir // (file |> Fpath.base))
                  in
                  Bos.OS.File.write file_name content |> Result.get_ok)
                files;
              Bos.OS.Dir.contents folder |> Result.get_ok
              |> List.map Fpath.to_string
              |> List.iter (fun s -> Logs.info (fun f -> f "Writing %s@." s));
              Ok Value.{ files }
        end

    let pp = Key.pp
    let auto_cancel = true
  end
end

module JCache = Current_cache.Make (Raw.Jobs)

let run_job ~local_src ~src ~inputs ~outputs job =
  let open Current.Syntax in
  Current.component "run job"
  |>
  let> local_commit = local_src and> k = job and> commit = src in
  let repo_path = Git.Commit.repo commit in
  let inputs = Fpath.(repo_path / inputs) in
  let outputs = Fpath.(repo_path / outputs) in
  JCache.get { local_commit; inputs; outputs } k

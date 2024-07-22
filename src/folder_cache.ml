open! Import
open Lwt.Infix

type t = Fpath.t * Fpath.t [@@deriving ord]

let pp f t =
  Fmt.string f
    (Format.sprintf "input: %s, config: %s"
       (t |> fst |> file_path (Fpath.v "inputs"))
       (t |> snd |> file_path (Fpath.v "inputs")))

(* let pp f t = Format.pp_print_list t_pp f t *)

module Raw = struct
  module Input = struct
    type t = { default_config : Fpath.t }

    let id = "Input-folder-cache"

    module Key = struct
      type t = { path : Fpath.t; commit : Git.Commit.t }

      let to_json t =
        `Assoc
          [
            ("dir", `String (t.path |> Fpath.to_string));
            ("commit", `String (t.commit |> Git.Commit.hash));
          ]

      let digest t = to_json t |> Yojson.Safe.to_string
      let pp f t = Yojson.Safe.pretty_print f (to_json t)
    end

    module Value = struct
      type t = { files : (Fpath.t * Fpath.t) list } [@@deriving ord]
      (* input path * config path *)

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
                       let fn = fst f and cfg = snd f in
                       `List
                         (`String (Fpath.to_string fn)
                         :: [ `String (Fpath.to_string cfg) ]))
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
                  | [ `String fn; `String ct ] -> (Fpath.v fn, Fpath.v ct)
                  | _ -> assert false)
                f
            in
            { files }
        | _ -> failwith "Failed to unmarshal files"
    end

    let rec read_input_folder ~folder ~default_config =
      let files = Bos.OS.Dir.contents folder |> Result.get_ok in
      List.fold_left
        (fun acc file ->
          if Bos.OS.Dir.exists file |> Result.get_ok then
            read_input_folder ~folder:file ~default_config @ acc
          else if Fpath.has_ext "csv" file then
            let current_folder = file |> Fpath.split_base |> fst in
            let config =
              List.find_opt
                (fun f ->
                  let folder, name = f |> Fpath.split_base in
                  folder = current_folder && name = Fpath.v "config")
                files
            in
            let config = Option.value ~default:default_config config in
            (file, config) :: acc
          else acc)
        [] files

    let build { default_config } (job : Current.Job.t) (k : Key.t) :
        Value.t Current.or_error Lwt.t =
      let { path; commit = _ } : Key.t = k in
      Current.Job.start ~level:Dangerous job >>= fun () ->
      Ok Value.{ files = read_input_folder ~folder:path ~default_config }
      |> Lwt.return
      >|= fun res -> res

    let pp = Key.pp
    let auto_cancel = true
  end

  module Folder = struct
    type t = unit

    let id = "Folder-cache"

    module Key = struct
      type t = { path : Fpath.t; commit : Git.Commit.t }

      let to_json t =
        `Assoc
          [
            ("dir", `String (t.path |> Fpath.to_string));
            ("commit", `String (t.commit |> Git.Commit.hash));
          ]

      let digest t = to_json t |> Yojson.Safe.to_string
      let pp f t = Yojson.Safe.pretty_print f (to_json t)
    end

    module Value = struct
      type t = { files : Fpath.t list } [@@deriving ord]

      let file_pp f t = Fpath.pp f t
      let pp f t = Format.pp_print_list file_pp f t.files

      let marshal t =
        let json =
          `Assoc
            [
              ( "files",
                `List (List.map (fun f -> `String (Fpath.to_string f)) t.files)
              );
            ]
        in
        Yojson.to_string json

      let unmarshal s =
        match Yojson.Safe.from_string s with
        | `Assoc [ ("files", `List f) ] ->
            let files =
              List.map
                (fun m ->
                  match m with `String fn -> Fpath.v fn | _ -> assert false)
                f
            in
            { files }
        | _ -> failwith "Failed to unmarshal files"
    end

    let rec read_folder ~folder =
      let files = Bos.OS.Dir.contents folder |> Result.get_ok in
      List.fold_left
        (fun acc file ->
          if Bos.OS.Dir.exists file |> Result.get_ok then
            read_folder ~folder:file @ acc
          else file :: acc)
        [] files

    let build _ (job : Current.Job.t) (k : Key.t) :
        Value.t Current.or_error Lwt.t =
      let { path; commit = _ } : Key.t = k in
      Current.Job.start ~level:Dangerous job >>= fun () ->
      Ok Value.{ files = read_folder ~folder:path } |> Lwt.return >|= fun res ->
      res

    let pp = Key.pp
    let auto_cancel = true
  end
end

module IFCache = Current_cache.Make (Raw.Input)
module FCache = Current_cache.Make (Raw.Folder)

let read_input_folder ?schedule ~where ~config src =
  let open Current.Syntax in
  Current.component "read folder: inputs/"
  |>
  let> commit = src in
  let repo_path = Git.Commit.repo commit in
  let path = Fpath.(repo_path / where) in
  let default_config = Fpath.(repo_path / config) in
  let k = Raw.Input.Key.{ path; commit } in
  IFCache.get ?schedule { default_config } k

let read_folder ?schedule ~label ~where src =
  let open Current.Syntax in
  Current.component "read folder: %a" Fmt.string label
  |>
  let> commit = src in
  let repo_path = Git.Commit.repo commit in
  let path = Fpath.(repo_path / where) in
  let k = Raw.Folder.Key.{ path; commit } in
  FCache.get ?schedule () k

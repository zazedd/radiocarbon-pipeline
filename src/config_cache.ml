open! Import
open Lwt.Infix

module Raw = struct
  module Config = struct
    type t = { script_files : Fpath.t list }

    let id = "Config-cache"

    module Key = struct
      type t = { path : Fpath.t; commit : Git.Commit.t }

      let to_json t =
        `Assoc
          [
            ("dir", `String (t.path |> Fpath.to_string));
            ("commit", `String (Git.Commit.hash t.commit));
          ]

      let digest t = to_json t |> Yojson.Safe.to_string
      let pp f t = Yojson.Safe.pretty_print f (to_json t)
    end

    module Value = struct
      type t = Fpath.t

      let pp f t = Fpath.pp f t

      let marshal t =
        let json = `Assoc [ ("path", `String (t |> Fpath.to_string)) ] in
        Yojson.to_string json

      let unmarshal s =
        match Yojson.Safe.from_string s with
        | `Assoc [ ("path", `String s) ] -> Fpath.v s
        | _ -> failwith "Failed to unmarshal files"
    end

    let build { script_files } (job : Current.Job.t) (k : Key.t) :
        Value.t Current.or_error Lwt.t =
      let { path; commit = _ } : Key.t = k in
      Current.Job.start ~level:Dangerous job >>= fun () ->
      (Bos.OS.File.read_lines path
       |> Result.get_ok
       |> List.map (fun s -> String.split_on_char '=' s)
       |> List.find (fun lst -> List.hd lst = "script")
       |> (function
            | [ _; script ] -> Ok (script |> Fpath.v)
            | _ -> Error (`Msg "No script name in the config file!"))
       |> Lwt.return
       >|= function
       | Error _ as e -> e
       | Ok script -> (
           match
             List.find_opt (fun sc -> Fpath.base sc = script) script_files
           with
           | None ->
               Error
                 (`Msg
                   "Script name does not match any file in the scripts/ folder")
           | Some s -> Ok s))
      >|= fun res -> res

    let pp = Key.pp
    let auto_cancel = true
  end
end

module CCache = Current_cache.Make (Raw.Config)

let grab_script ?schedule ~script_files ~fc src =
  let open Current.Syntax in
  Current.component "get script from config"
  |>
  let> commit = src and> _, path = fc in
  let k = Raw.Config.Key.{ path; commit } in
  CCache.get ?schedule { script_files } k

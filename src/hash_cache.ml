open! Import
open Lwt.Infix

module Raw = struct
  module Hashes = struct
    type t = unit

    let id = "Hash-cache"

    module Key = struct
      type t = {
        input : Fpath.t;
        config : Fpath.t;
        script : Fpath.t;
        commit : Git.Commit.t;
      }

      let to_json t =
        `Assoc
          [
            ("file", `String (t.input |> Fpath.to_string));
            ("config", `String (t.config |> Fpath.to_string));
            ("script", `String (t.script |> Fpath.to_string));
            ("commit", `String (t.commit |> Git.Commit.hash));
          ]

      let digest t = to_json t |> Yojson.Safe.to_string
      let pp f t = Yojson.Safe.pretty_print f (to_json t)
    end

    module Value = struct
      type t = {
        script : Fpath.t * string;
        input : Fpath.t * string;
        config : Fpath.t * string;
      }

      let marshal t =
        let script, input, config = (t.script, t.input, t.config) in
        let json =
          `Assoc
            [
              ( "script",
                `List
                  [
                    `String (script |> fst |> Fpath.to_string);
                    `String (script |> snd);
                  ] );
              ( "input",
                `List
                  [
                    `String (input |> fst |> Fpath.to_string);
                    `String (input |> snd);
                  ] );
              ( "config",
                `List
                  [
                    `String (config |> fst |> Fpath.to_string);
                    `String (config |> snd);
                  ] );
            ]
        in
        Yojson.to_string json

      let unmarshal s =
        match Yojson.Safe.from_string s with
        | `Assoc
            [
              ("script", `List [ `String s_p; `String s_h ]);
              ("input", `List [ `String i_p; `String i_h ]);
              ("config", `List [ `String c_p; `String c_h ]);
            ] ->
            {
              script = (s_p |> Fpath.v, s_h);
              input = (i_p |> Fpath.v, i_h);
              config = (c_p |> Fpath.v, c_h);
            }
        | _ -> failwith "Failed to unmarshal files"
    end

    let build _ (job : Current.Job.t) (k : Key.t) :
        Value.t Current.or_error Lwt.t =
      let { input; config; script; commit = _ } : Key.t = k in
      Current.Job.start ~level:Dangerous job >>= fun () ->
      Format.printf "HERE@.";
      let input_dgst =
        Bos.OS.File.read input |> Result.get_ok |> Digestif.SHA512.digest_string
        |> Digestif.SHA512.to_hex
      in
      Format.printf "HERE2@.";
      let config_dgst =
        Bos.OS.File.read config |> Result.get_ok
        |> Digestif.SHA512.digest_string |> Digestif.SHA512.to_hex
      in
      Format.printf "HERE3 %s@." (script |> Fpath.to_string);
      let script_dgst =
        Bos.OS.File.read script |> Result.get_ok
        |> Digestif.SHA512.digest_string |> Digestif.SHA512.to_hex
      in
      Format.printf "HERE4@.";
      Ok
        Value.
          {
            script = (script, script_dgst);
            input = (input, input_dgst);
            config = (config, config_dgst);
          }
      |> Lwt.return
      >|= fun res -> res

    let pp = Key.pp
    let auto_cancel = true
  end
end

module HashCash = Current_cache.Make (Raw.Hashes)

let get_paths_and_hashes ?schedule ~src ~fc script =
  let open Current.Syntax in
  Current.component "get hashes"
  |>
  let> commit = src and> input, config = fc and> script = script in
  let k = Raw.Hashes.Key.{ input; config; script; commit } in
  HashCash.get ?schedule () k

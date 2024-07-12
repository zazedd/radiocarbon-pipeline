open! Import
open Lwt.Infix

module Raw = struct
  module Config = struct
    type t = unit

    let id = "Config-cache"

    module Key = struct
      type t = { path : Fpath.t; contents : string list }

      let to_json t =
        `Assoc
          [
            ("dir", `String (t.path |> Fpath.to_string));
            ("contents", [%derive.to_yojson: string list] t.contents);
          ]

      let digest t = to_json t |> Yojson.Safe.to_string
      let pp f t = Yojson.Safe.pretty_print f (to_json t)
    end

    module Value = Current.String

    let build _ (job : Current.Job.t) (k : Key.t) :
        Value.t Current.or_error Lwt.t =
      let { path = _; contents } : Key.t = k in
      Current.Job.start ~level:Dangerous job >>= fun () ->
      contents
      |> List.map (fun s -> String.split_on_char '=' s)
      |> List.find (fun lst -> List.hd lst = "script")
      |> (function
           | [ _; script ] -> Ok script
           | _ -> Error (`Msg "Unsupported script arguments"))
      |> Lwt.return
      >|= fun res -> res

    let pp = Key.pp
    let auto_cancel = true
  end
end

module CCache = Current_cache.Make (Raw.Config)

let grab_script ~path ~contents =
  let open Current.Syntax in
  let k = Raw.Config.Key.{ path; contents } in
  Current.component "read config"
  |>
  let> () = Current.return () in
  CCache.get () k

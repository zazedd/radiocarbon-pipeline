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
end

module HashCash = Current_cache.Make (Raw.Git_hash)

(* fix prefixes, /var/folders/.../inputs/... != inputs/... *)
let fix_prefixes fl dir =
  List.map
    (fun (p, d) ->
      let folder = p |> Fpath.split_base |> fst in
      let file = Fpath.base p in
      if folder = Fpath.v "inputs/" then (Fpath.(dir // file), d)
      else (Fpath.(dir // folder // file), d))
    fl

let old_hashes = ref []

let grab_new_and_changed (new_hash : (Fpath.t * string) list Current.t) dir =
  let open Current.Syntax in
  let+ new_hashes = new_hash in
  let new_hashes = fix_prefixes new_hashes dir in
  let new_hashes =
    (*
       if it is not a csv then its a config file. lets leave it alone for now
       if it is a csv, lets traverse the list 
       and find the config file that is in the same dir
       if there is no config file then None, else Some something. 'None' gets handled later
    *)
    List.fold_left
      (fun acc (p, d) ->
        if Fpath.has_ext "csv" p then
          let current_folder = p |> Fpath.split_base |> fst in
          let config =
            List.find_opt
              (fun (f, _) ->
                let folder = f |> Fpath.split_base |> fst in
                folder = current_folder && f <> p)
              new_hashes
          in
          ((p, d), config) :: acc
        else acc)
      [] new_hashes
  in
  if !old_hashes <> new_hashes then (
    let changed_and_new =
      List.filter (fun file -> List.mem file !old_hashes |> not) new_hashes
    in
    old_hashes := new_hashes;
    Some changed_and_new)
  else None

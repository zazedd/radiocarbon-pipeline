open Current.Syntax
open Current_nix.Default
module NixCache = Current_cache.Make (Current_nix.Nix_cmd)

let shell ?level ?schedule ?timeout ?label ?flake ?path ?pool ~args src =
  (*
      [
        [ "Rscript"; "--version" ];
        [ "echo"; "hello" ];
      ]
      ->
      [ "-c"; "bash"; "-c" "Rscript --version && echo hello" ];
    *)
  let lock =
    match Unix.access "./flake.lock" [ Unix.F_OK ] with
    | () -> Some (Fpath.v "./flake.lock")
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> None
  in
  let args =
    List.mapi
      (fun i arg_list ->
        if i <> List.length args - 1 then arg_list @ [ "&&" ] else arg_list)
      args
    |> List.fold_left
         (fun acc arg_list ->
           acc ^ List.fold_left (fun acc2 arg -> acc2 ^ " " ^ arg) "" arg_list)
         ""
  in
  let args = [ "-c"; "bash"; "-c"; args ] in
  Current.component "shell%a" pp_sp_label label
  |>
  let> commit = get_build_context src
  and> flake_ext = Current.option_seq flake in
  let flake_path =
    match flake_ext with
    | None -> `Path (Fpath.v ".", "")
    | Some (`Path _ as f) -> f
    | Some (`Contents c) -> `Contents c
  in
  let k =
    {
      Current_nix.Nix_cmd.Key.commit;
      flake = flake_path;
      lock;
      command = `Develop;
      args;
      path;
    }
  in
  NixCache.invalidate k;
  let res = NixCache.get ?schedule { pool; timeout; level } k in
  NixCache.invalidate k;
  res

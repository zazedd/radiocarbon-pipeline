module Github = Current_github
module Git = Current_git

let ( / ) = Filename.concat
let ( >>!= ) = Lwt_result.( >>= )
let or_raise = function Ok v -> v | Error (`Msg m) -> failwith m

type status =
  [ `No_changes
  | `Csv_changes
  | `Config_changes
  | `Script_changes
  | `Multiple_changes ]

let set_commit_status commit description status check_name =
  let status =
    Github.Api.CheckRunStatus.v ?text:(Some description) status
    |> Current.return
  in
  Github.Api.CheckRun.set_status commit check_name status

let rec get_suffix path cmp acc =
  if path = Fpath.v "/" then Fpath.base acc
  else if path = cmp then acc
  else
    let new_path, b = Fpath.split_base path in
    if cmp = b then acc
    else get_suffix (new_path |> Fpath.rem_empty_seg) cmp Fpath.(b // acc)

let file_path cmp t =
  let input_dir, name = Fpath.split_base t in
  let input_dir = input_dir |> Fpath.rem_empty_seg in
  Format.printf "%s, %s@."
    (input_dir |> Fpath.to_string)
    (name |> Fpath.to_string);
  get_suffix input_dir cmp name |> Fpath.to_string

let rec base_dir path =
  let a = Fpath.split_base path |> fst in
  if a = Fpath.v "./" then path else base_dir a
